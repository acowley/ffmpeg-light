-- This module provides API for copying image data from AVFrame.
-- Also FFI declarations for the underlying FFmpeg functions.
module Codec.FFmpeg.Buffer
( av_image_get_buffer_size
, av_image_copy_to_buffer
, copyImage
) where

import Codec.FFmpeg.Common
import Codec.FFmpeg.Enums 
import Codec.FFmpeg.Types

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)

import Foreign.C.Types
import Foreign.Ptr


-- Returns a required size of buffer to hold image data.
foreign import ccall "av_image_get_buffer_size"
  av_image_get_buffer_size
    -- Desired pixel format.
    :: AVPixelFormat
    -- Width.
    -> CInt
    -- Height.
    -> CInt
    -- Linesize alignment.
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
    -- Line size alignment of destination.
    -> CInt
    -- Number of bytes written to destination.
    -> IO CInt


-- Returns line size align.
lineSizeAlign :: CInt -> CInt
lineSizeAlign lineSize
  -- Greater cases are 128 and 256 which are multiple of 32.
  | lineSize `mod` 32 == 0 = 32
  | lineSize `mod` 16 == 0 = 16
  | lineSize `mod` 8  == 0 = 8
  | lineSize `mod` 4  == 0 = 4
  | lineSize `mod` 2  == 0 = 2
  | otherwise              = 1
  
  
{- Returns a byte-string containing an image data.
   
   Returned ByteString doesn't refer back to it's
   source AVFrame. So, source frame may be deleted
   or changed, but image will stay.
   
   Returned ByteString may be used to fill up SDL's
   textures (using SDL.updateTexture).
   
   I'm not sure about using unsafePerformIO here.
-}
copyImage :: AVFrame -> IO ByteString
copyImage frame =
  do
    
    -- Get width and height of frame.
    width  <- getWidth  frame
    height <- getHeight frame
    
    -- Frame pixel format.
    pixelFormat <- getPixelFormat frame
    
    -- Get size of a line in bytes including padding.
    lineSize <- getLineSize frame
    
    -- Get line size alignment.
    let align = lineSizeAlign lineSize
    
    -- Get required size of buffer to hold image data.
    imageBufSize <- av_image_get_buffer_size
                      pixelFormat
                      width
                      height
                      align
                         
    -- Allocate buffer to hold image data.
    imageBuf <- av_malloc $ fromIntegral imageBufSize
    
    -- Copy image to buffer.
    _ <- av_image_copy_to_buffer
          (castPtr imageBuf)
          imageBufSize
          -- AVFrame's data pointers.
          (castPtr $ hasData frame)
          -- AVFrame's size pointers. 
          (hasLineSize frame)
          pixelFormat
          width
          height
          -- Same align for destination.
          align
          
    -- Image data buffer cleanup.
    let imageBufCleanup = av_free imageBuf
    
    -- Fill up byte-string image by data from buffer.
    image <- unsafePackCStringFinalizer
               (castPtr imageBuf)
               (fromIntegral imageBufSize)
               -- Cleanup for buffer.
               imageBufCleanup
               
    return image
