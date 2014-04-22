{-# LANGUAGE FlexibleContexts, ForeignFunctionInterface #-}
module Codec.FFmpeg.Common where
import Codec.FFmpeg.Enums
import Codec.FFmpeg.Types
import Control.Monad (when)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.Ptr

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

-- * Utility functions

-- | Catch an IOException from an IO action and re-throw it in a
-- wrapping monad transformer.
wrapIOError :: (MonadIO m, Error e, MonadError e m) => IO a -> m a
wrapIOError = liftIO . flip catchError (errMsg . show)

-- | Throw an error with a 'String' message.
errMsg :: (Error e, MonadError e m) => String -> m a
errMsg = throwError . strMsg

-- * Wrappers that may throw 'IOException's.

-- | Allocate an 'AVFrame' and set its fields to default values.
frame_alloc_check :: IO AVFrame
frame_alloc_check = do r <- av_frame_alloc
                       when (getPtr r == nullPtr)
                            (errMsg "Couldn't allocate frame")
                       return r

-- | Allocate new buffer(s) for audio or video data with the required
-- alignment. Note, for video frames, pixel format, @width@, and
-- @height@ must be set before calling this function. For audio
-- frames, sample @format@, @nb_samples@, and @channel_layout@ must be
-- set.
frame_get_buffer_check :: AVFrame -> CInt -> IO ()
frame_get_buffer_check f x = do r <- av_frame_get_buffer f x
                                when (r /= 0)
                                     (errMsg "Failed to allocate buffers")
