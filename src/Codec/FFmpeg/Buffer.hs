-- This module provides API for copying image data from AVFrame.
-- Also FFI declarations for the underlying FFmpeg functions.
module Codec.FFmpeg.Buffer
( av_image_get_buffer_size
, av_image_copy_to_buffer
, copyImage
, lineSizeAlign
, frameBufferSize
, frameCopyToBuffer
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
    -- Line size alignment of source image.
    -> CInt
    -- Number of bytes written to destination.
    -> IO CInt


-- Returns line size alignment. It a width of frame as parameter.
lineSizeAlign :: CInt -> CInt
lineSizeAlign width
  -- Greater cases are 128 and 256 which are multiple of 32.
  | width `mod` 32 == 0 = 32
  | width `mod` 16 == 0 = 16
  | width `mod` 8  == 0 = 8
  | otherwise           = 1
  
  
-- Wrapper for av_image_get_buffer_size.
frameBufferSize :: AVFrame -> IO CInt
frameBufferSize frame =
  do
    
    -- Frame pixel format.
    pixelFormat <- getPixelFormat frame

    -- Get width and height of frame.
    width  <- getWidth  frame
    height <- getHeight frame
    
    -- Alignment of line size.
    let alignment = lineSizeAlign width
    
    -- Supply everything above to av_image_get_buffer_size. 
    av_image_get_buffer_size
      pixelFormat
      width
      height
      alignment
      
      
-- Wrapper for av_image_copy_to_buffer. It is assumed that size
-- of destination buffer is equal to (frameBufferSize givenFrame).
frameCopyToBuffer :: AVFrame -> Ptr CUChar -> IO CInt
frameCopyToBuffer frame buffer =
  do
    
    -- Destination buffer size.
    bufferSize <- frameBufferSize frame
    -- Source image data.
    let imageData = hasData frame
    -- Source image line size.
    let lineSize = hasLineSize frame
    -- Source image pixel format.
    pixelFormat <- getPixelFormat frame
    -- Source image width.
    width <- getWidth  frame
    -- Source image height.
    height <- getHeight frame
    -- Line size alignment.
    let alignment = lineSizeAlign width
    
    -- Supply everything above to av_image_copy_to_buffer.
    av_image_copy_to_buffer
      buffer
      bufferSize
      (castPtr imageData)
      lineSize
      pixelFormat
      width
      height
      alignment
    

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
    
    -- Get required size of buffer to hold image data.
    imageBufSize <- frameBufferSize frame
                         
    -- Allocate buffer to hold image data.
    imageBuf <- av_malloc $ fromIntegral imageBufSize
    
    -- Image data buffer cleanup.
    let imageBufCleanup = av_free imageBuf
    
    -- Copy image to buffer.
    _ <- frameCopyToBuffer frame (castPtr imageBuf)
    
    -- Fill up byte-string image by data from buffer.
    image <- unsafePackCStringFinalizer
               (castPtr imageBuf)
               (fromIntegral imageBufSize)
               -- Cleanup for buffer.
               imageBufCleanup
               
    return image
