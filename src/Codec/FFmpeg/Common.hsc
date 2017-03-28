{-# LANGUAGE FlexibleContexts, ForeignFunctionInterface #-}
module Codec.FFmpeg.Common where
import Codec.FFmpeg.Enums
import Codec.FFmpeg.Types
import Control.Monad (when)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.Ptr
import Control.Monad.Trans.Maybe

foreign import ccall "avcodec_open2"
  open_codec :: AVCodecContext -> AVCodec -> Ptr AVDictionary -> IO CInt

foreign import ccall "av_frame_alloc"
  av_frame_alloc :: IO AVFrame

foreign import ccall "av_frame_get_buffer"
  av_frame_get_buffer :: AVFrame -> CInt -> IO CInt

foreign import ccall "av_frame_free"
  av_frame_free :: Ptr AVFrame -> IO ()

foreign import ccall "avcodec_close"
  codec_close :: AVCodecContext -> IO CInt

foreign import ccall "av_init_packet"
  init_packet :: AVPacket -> IO ()

foreign import ccall "av_free_packet"
  free_packet :: AVPacket -> IO ()

foreign import ccall "av_malloc"
  av_malloc :: CSize -> IO (Ptr ())

foreign import ccall "av_free"
  av_free :: Ptr () -> IO ()

foreign import ccall "sws_getCachedContext"
  sws_getCachedContext :: SwsContext
                       -> CInt -> CInt -> AVPixelFormat
                       -> CInt -> CInt -> AVPixelFormat
                       -> SwsAlgorithm -> Ptr () -> Ptr () -> Ptr CDouble
                       -> IO SwsContext

foreign import ccall "sws_scale"
  sws_scale :: SwsContext
            -> Ptr (Ptr CUChar) -> Ptr CInt -> CInt -> CInt
            -> Ptr (Ptr CUChar) -> Ptr CInt -> IO CInt

-- Return size of buffer for image.
foreign import ccall "av_image_get_buffer_size"
  av_image_get_buffer_size
    -- Pixel format.
    :: AVPixelFormat
    -- Width.
    -> CInt
    -- Height.
    -> CInt
    -- Line size alignment.
    -> CInt
    -- Size of buffer.
    -> IO CInt
    
-- Copy image to buffer.
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
    -- Source image line size alignment.
    -> CInt
    -- Number of bytes written to destination.
    -> IO CInt
    
    
-- * Utility functions

-- | Catch an IOException from an IO action and re-throw it in a
-- wrapping monad transformer.
wrapIOError :: (MonadIO m, MonadError String m) => IO a -> m a
wrapIOError io = liftIO (catchError (fmap Right io) (return . Left . show))
                 >>= either throwError return

-- * Wrappers that may throw 'IOException's.

-- | Allocate an 'AVFrame' and set its fields to default values.
frame_alloc_check :: IO AVFrame
frame_alloc_check = do r <- av_frame_alloc
                       when (getPtr r == nullPtr)
                            (error "Couldn't allocate frame")
                       return r

-- | Allocate new buffer(s) for audio or video data with the required
-- alignment. Note, for video frames, pixel format, @width@, and
-- @height@ must be set before calling this function. For audio
-- frames, sample @format@, @nb_samples@, and @channel_layout@ must be
-- set.
frame_get_buffer_check :: AVFrame -> CInt -> IO ()
frame_get_buffer_check f x = do r <- av_frame_get_buffer f x
                                when (r /= 0)
                                     (error "Failed to allocate buffers")

-- | Bytes-per-pixel for an 'AVPixelFormat'
avPixelStride :: AVPixelFormat -> Maybe Int
avPixelStride fmt
  | fmt == avPixFmtGray8  = Just 1
  | fmt == avPixFmtRgb24  = Just 3
  | fmt == avPixFmtRgba   = Just 4
  | fmt == avPixFmtRgb8   = Just 1
  | fmt == avPixFmtPal8   = Just 1
  | otherwise = Nothing
  
-- | Return line size alignment.
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

-- | Retun 'AVFrame's line size.
frameLineSize :: AVFrame -> IO (Maybe CInt)
frameLineSize frame = do
  w   <- getWidth frame
  fmt <- getPixelFormat frame  
  return $
    (*w) . fromIntegral <$> avPixelStride fmt

-- | Transformer version of 'frameLineSize'.
frameLineSizeT :: AVFrame -> MaybeT IO CInt
frameLineSizeT = MaybeT . frameLineSize 

-- Return 'AVFrame's alignment.
frameAlign :: AVFrame -> IO (Maybe CInt)
frameAlign = fmap (fmap lineSizeAlign) . frameLineSize 

-- Transformer version of 'frameAlign'.
frameAlignT :: AVFrame -> MaybeT IO CInt
frameAlignT = MaybeT . frameAlign


-- * Wrappers for copying 'AVFrame's image to buffer.    
        
-- | Return size of buffer for 'AVFrame's image.
frameBufferSize :: AVFrame -> IO (Maybe CInt)
frameBufferSize frame =
  runMaybeT $ do
    a <- frameAlignT frame
    MaybeT $ do
      fmt <- getPixelFormat frame
      w   <- getWidth frame
      h   <- getHeight frame
      Just <$> av_image_get_buffer_size fmt w h a

-- | Transformer version of 'frameBufferSize'.
frameBufferSizeT :: AVFrame -> MaybeT IO CInt
frameBufferSizeT = MaybeT . frameBufferSize

-- | Copy 'AVFrame's image to buffer.      
-- It is assumed that size of buffer is equal to
--
-- > bufSize <- fromJust <$> frameBufferSize frame.
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
          
-- | Transformer version of 'frameCopyToBuffer'.
frameCopyToBufferT :: AVFrame -> Ptr CUChar -> MaybeT IO CInt
frameCopyToBufferT frame = MaybeT . frameCopyToBuffer frame
