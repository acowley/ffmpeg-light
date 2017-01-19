-- This module provides API for copying image data from AVFrame.
-- Also FFI declarations for the underlying FFmpeg functions.
module Codec.FFmpeg.Buffer where

import Codec.FFmpeg.Common
import Codec.FFmpeg.Enums 
import Codec.FFmpeg.Types

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)

import Foreign.C.Types
import Foreign.Ptr

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

-- Returns a required size of buffer to hold image data.
foreign import ccall "av_image_get_buffer_size"
  av_image_get_buffer_size
    -- Desired pixel format.
    :: AVPixelFormat
    -- Width.
    -> CInt
    -- Height.
    -> CInt
    -- Line size alignment.
    -> CInt
    -- Size of buffer.
    -> IO CInt
    
    
-- Fills up a buffer by image data.
foreign import ccall "av_image_copy_to_buffer"
  av_image_copy_to_buffer
    -- Destination buffer.
    :: Ptr CUChar
    -- Destination buffer size.
    -> CInt
    -- Source image data.
    -> Ptr (Ptr CUChar)
    -- Source image line size.
    -> Ptr CInt
    -- Source image pixel format.
    -> AVPixelFormat
    -- Source image width.
    -> CInt
    -- Source image height.
    -> CInt
    -- Line size alignment of destination image.
    -> CInt
    -- Number of bytes written to destination.
    -> IO CInt


-- Returns line size alignment.
lineSizeAlign :: CInt -> CInt
lineSizeAlign lineSize
  -- Alignment for 512 bit register.
  | lineSize `mod` 64 == 0 = 64
  -- Alignment for 256 bit register.
  | lineSize `mod` 32 == 0 = 32
  -- Alignment for 128 bit register.
  | lineSize `mod` 16 == 0 = 16
  -- Alignment for 64 bit register.
  | lineSize `mod` 8  == 0 = 8
  -- Alignment for 32 bit register.
  | lineSize `mod` 4  == 0 = 4
  -- Alignment for 16 bit register.
  | lineSize `mod` 2  == 0 = 2
  -- Alignment for 8 bit register.
  | otherwise = 1


-- Returns frame's image alignment.
frameAlign :: AVFrame -> IO (Maybe CInt)
frameAlign = runMaybeT . frameAlignT

-- Transformer version of frameAlign.
frameAlignT :: AVFrame -> MaybeT IO CInt
frameAlignT frame =
  MaybeT $ do
    fmt <- getPixelFormat frame
    w   <- getWidth frame
    return $
      avPixelStride fmt >>=
        return . lineSizeAlign . (*w) . fromIntegral
        
      
-- Wrapper for av_image_get_buffer_size.
frameBufferSize :: AVFrame -> IO (Maybe CInt)
frameBufferSize frame =
  runMaybeT $ do
    a <- frameAlignT frame
    MaybeT $ do
      fmt <- getPixelFormat frame
      w   <- getWidth frame
      h   <- getHeight frame
      Just <$> av_image_get_buffer_size fmt w h a

-- Transformer version of frameBufferSize.
frameBufferSizeT :: AVFrame -> MaybeT IO CInt
frameBufferSizeT = MaybeT . frameBufferSize

      
-- Wrapper for av_image_copy_to_buffer. It is assumed that size
-- of destination buffer is equal to (frameBufferSize givenFrame).
frameCopyToBuffer :: AVFrame -> Ptr CUChar -> IO (Maybe CInt)
frameCopyToBuffer frame buffer =
  runMaybeT $ do
  
    a <- frameAlignT frame
    s <- frameBufferSizeT frame
    
    MaybeT $ do
        
      let imageData = hasData frame
          lineSize  = hasLineSize frame
      
      fmt <- getPixelFormat frame
      w   <- getWidth frame
      h   <- getHeight frame

      Just <$>
        av_image_copy_to_buffer
          buffer
          s
          (castPtr imageData)
          lineSize
          fmt
          w
          h
          a

-- Transformer version of frameCopyToBuffer.
frameCopyToBufferT :: AVFrame -> Ptr CUChar -> MaybeT IO CInt
frameCopyToBufferT frame = MaybeT . frameCopyToBuffer frame
  

{- Returns a byte-string containing an image data.
   
   Returned ByteString doesn't refer back to it's
   source AVFrame. So, source frame may be deleted
   or changed, but image will stay.
   
   Returned ByteString may be used to fill up SDL's
   textures (using SDL.updateTexture).
   
   I'm not sure about using unsafePerformIO here.
-}
copyImage :: AVFrame -> IO (Maybe ByteString)
copyImage frame =
  runMaybeT $ do
    
    -- Get required size of buffer to hold image data.
    imageBufSize <- frameBufferSizeT frame
                         
    -- Allocate buffer to hold image data.
    imageBuf <- MaybeT $
       Just <$> (av_malloc $ fromIntegral imageBufSize)
    
    -- Image data buffer cleanup.
    let imageBufCleanup = av_free imageBuf
    
    -- Copy image to buffer.
    frameCopyToBufferT frame (castPtr imageBuf)
    
    -- Fill up byte-string image by data from buffer.
    MaybeT $ Just <$>
      unsafePackCStringFinalizer
        (castPtr imageBuf)
        (fromIntegral imageBufSize)
        -- Cleanup for buffer.
        imageBufCleanup
  
