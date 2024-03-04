{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Codec.FFmpeg.Common where
import           Codec.FFmpeg.Enums
import           Codec.FFmpeg.Types
import           Control.Exception
import           Control.Monad             (when)
import           Control.Monad.Error.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc     (allocaBytes, free)
import           Foreign.Marshal.Array     (advancePtr, mallocArray)
import           Foreign.Ptr
import           Foreign.Storable

-- | libavdevice still requries registration
foreign import ccall "avdevice_register_all"
  avdevice_register_all :: IO ()

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

foreign import ccall "av_packet_alloc"
  av_packet_alloc :: IO AVPacket

foreign import ccall "av_packet_unref"
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

foreign import ccall "swr_alloc"
  swr_alloc :: IO SwrContext

foreign import ccall "swr_init"
  swr_init :: SwrContext -> IO CInt

foreign import ccall "av_opt_set_int"
  av_opt_set_int :: Ptr () -> CString -> CLong -> CInt -> IO CInt

foreign import ccall "av_opt_get_int"
  av_opt_get_int :: Ptr () -> CString -> CInt -> Ptr CULong -> IO CInt

foreign import ccall "av_opt_set_sample_fmt"
  av_opt_set_sample_fmt :: Ptr () -> CString -> AVSampleFormat -> CInt -> IO CInt

foreign import ccall "av_opt_get_sample_fmt"
  av_opt_get_sample_fmt :: Ptr () -> CString -> CInt -> Ptr AVSampleFormat -> IO CInt

foreign import ccall "av_opt_get_chlayout"
  av_opt_get_chlayout :: Ptr () -> CString -> CInt -> Ptr AVChannelLayout -> IO CInt

foreign import ccall "av_opt_set_chlayout"
  av_opt_set_chlayout :: Ptr () -> CString -> AVChannelLayout -> CInt -> IO CInt

foreign import ccall "avcodec_send_frame"
  avcodec_send_frame :: AVCodecContext -> AVFrame -> IO CInt

foreign import ccall "avcodec_send_packet"
  avcodec_send_packet :: AVCodecContext -> AVPacket -> IO CInt

foreign import ccall "avcodec_receive_frame"
  avcodec_receive_frame :: AVCodecContext -> AVFrame -> IO CInt

foreign import ccall "avcodec_receive_packet"
  avcodec_receive_packet :: AVCodecContext -> AVPacket -> IO CInt

foreign import ccall "av_get_channel_name"
  av_get_channel_name :: CULong -> IO CString

foreign import ccall "av_get_channel_description"
  av_get_channel_description :: CULong -> IO CString

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

newtype FFmpegException = FFmpegException String deriving Show

instance Exception FFmpegException

runWithError :: String -> IO CInt -> IO CInt
runWithError msg toRun = do
  r <- toRun
  when (r < 0) (getError msg r)
  return r

getError msg r = do
  let len = 100 -- I have no idea how long this string should be so this is a guess
  errCStr <- mallocArray len
  av_strerror r errCStr (fromIntegral len)
  errStr <- peekCString errCStr
  free errCStr
  avError $ msg ++ " : " ++ errStr

avError :: String -> IO a
avError msg = throwIO $ FFmpegException $ msg

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

-- * FFmpeg Errors

foreign import ccall "av_strerror"
  av_strerror :: CInt -> Ptr CChar -> CSize -> IO CInt

stringError :: CInt -> IO String
stringError err =
  allocaBytes len $ \block -> do
    let buf = castPtr block
    _ <- av_strerror err buf (fromIntegral len)
    peekCString buf
  where
    len = 1000

-- | Walk a C array placing the values into a Haskell list.
-- Stop incrementing the pointer when the function returns True
walkPtrs :: Storable a
         => Ptr a -- ^ Ptr to the beginning of an array
         -> (Ptr a -> IO Bool) -- ^ Function to specify when we should terminate
         -> IO [a]
walkPtrs ptr isDone = do
  d <- isDone ptr
  if d
    then return []
    else do
      v <- peek ptr
      rest <- walkPtrs (advancePtr ptr 1) isDone
      return $ v : rest

listSupportedSampleFormats :: AVCodec -> IO [AVSampleFormat]
listSupportedSampleFormats codec = do
  fmts <- getSampleFormats codec
  walkPtrs fmts (\ptr ->
                    if ptr == nullPtr
                      then return True
                      else do
                        v <- peek ptr
                        return $ getSampleFormatInt v == -1
                )

listSupportedChannelLayouts :: AVCodec -> IO [CULong]
listSupportedChannelLayouts codec = do
  chanPtr <- getChannelLayouts codec
  walkPtrs chanPtr (\ptr ->
                    if ptr == nullPtr
                      then return True
                      else do
                        v <- peek ptr
                        return $ v == 0
                   )

listSupportedSampleRates :: AVCodec -> IO [CInt]
listSupportedSampleRates codec = do
  srPtr <- getSupportedSampleRates codec
  walkPtrs srPtr (\ptr ->
                    if ptr == nullPtr
                      then return True
                      else do
                        v <- peek ptr
                        return $ v == 0
                 )

first3 :: (t -> a) -> (t, b, c) -> (a, b, c)
first3 f (a,b,c) = (f a,b,c)