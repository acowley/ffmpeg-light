{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}
-- | Video decoding API. Includes FFI declarations for the underlying
-- FFmpeg functions, wrappers for these functions that wrap error
-- condition checking, and high level Haskellized interfaces.
module Codec.FFmpeg.Decode where

import           Codec.FFmpeg.AudioStream
import           Codec.FFmpeg.Common
import           Codec.FFmpeg.Enums
import           Codec.FFmpeg.Scaler
import           Codec.FFmpeg.Types
import           Control.Arrow             (first)
import           Control.Monad             (void, when)
import           Control.Monad.Except
import           Control.Monad.Trans.Maybe
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc     (alloca, free, mallocBytes)
import           Foreign.Marshal.Array     (advancePtr)
import           Foreign.Marshal.Utils     (with)
import           Foreign.Ptr
import           Foreign.Storable

-- * FFI Declarations

foreign import ccall "avformat_open_input"
  avformat_open_input :: Ptr AVFormatContext -> CString -> Ptr AVInputFormat
                      -> Ptr AVDictionary -> IO CInt

foreign import ccall "avformat_find_stream_info"
  avformat_find_stream_info :: AVFormatContext -> Ptr () -> IO CInt

foreign import ccall "av_find_best_stream"
  av_find_best_stream :: AVFormatContext -> AVMediaType -> CInt -> CInt
                      -> Ptr AVCodec -> CInt -> IO CInt

foreign import ccall "avcodec_find_decoder"
   avcodec_find_decoder :: AVCodecID -> IO AVCodec

foreign import ccall "avcodec_find_decoder_by_name"
  avcodec_find_decoder_by_name :: CString -> IO AVCodec

foreign import ccall "avpicture_get_size"
  avpicture_get_size :: AVPixelFormat -> CInt -> CInt -> IO CInt

foreign import ccall "av_malloc"
  av_malloc :: CSize -> IO (Ptr ())

foreign import ccall "av_read_frame"
  av_read_frame :: AVFormatContext -> AVPacket -> IO CInt

foreign import ccall "avcodec_decode_audio4"
  decode_audio :: AVCodecContext -> AVFrame -> Ptr CInt -> AVPacket
               -> IO CInt

foreign import ccall "avcodec_decode_video2"
  decode_video :: AVCodecContext -> AVFrame -> Ptr CInt -> AVPacket
               -> IO CInt
foreign import ccall "avformat_close_input"
  close_input :: Ptr AVFormatContext -> IO ()

foreign import ccall "av_dict_set"
  av_dict_set :: Ptr AVDictionary -> CString -> CString -> CInt -> IO CInt

foreign import ccall "av_find_input_format"
  av_find_input_format :: CString -> IO (Ptr AVInputFormat)

foreign import ccall "av_format_set_video_codec"
  av_format_set_video_codec :: AVFormatContext -> AVCodec -> IO ()

dictSet :: Ptr AVDictionary -> String -> String -> IO ()
dictSet d k v = do
  r <- withCString k $ \k' -> withCString v $ \v' ->
         av_dict_set d k' v' 0
  when (r < 0) $
    stringError r >>= \err ->
       error $ "av_dict_set failed("++ err ++"): "++k++" => "++v

-- * FFmpeg Decoding Interface

-- | Open the first video input device enumerated by FFMPEG.
openCamera :: (MonadIO m, MonadError String m) => String -> CameraConfig -> m AVFormatContext
openCamera cam cfg =
  wrapIOError . alloca $ \ctx ->
    withCString cam $ \cstr ->
      do avPtr <- mallocAVFormatContext
         setupCamera avPtr cam
         poke ctx avPtr
         fmt <- case format cfg of
                  Just "mjpeg" -> withCString "v4l2" av_find_input_format
                  Just f -> withCString f av_find_input_format
                  Nothing -> pure nullPtr
         r <- alloca $ \dict -> do
                setConfig dict cfg
                avformat_open_input ctx cstr fmt dict
         when (r /= 0) $
           stringError r >>= \err ->
             fail ("ffmpeg failed opening file: " ++ err)
         peek ctx
  where
    run :: (a -> IO b) -> Maybe a -> IO ()
    run _ Nothing  = return ()
    run f (Just x) = void (f x)

    setConfig :: Ptr AVDictionary -> CameraConfig -> IO ()
    setConfig dict (CameraConfig {..}) =
      do run (dictSet dict "framerate" . show) framerate
         run (\(w,h) -> dictSet dict "video_size" (show w ++ "x" ++ show h)) resolution

    setupCamera :: AVFormatContext -> String -> IO ()
    setupCamera avfc c =
      do setCamera avfc
         setFilename avfc c
         when (format cfg == Just "mjpeg") $ do
           mjpeg <- avcodec_find_decoder avCodecIdMjpeg
           setVideoCodecID avfc avCodecIdMjpeg
           av_format_set_video_codec avfc mjpeg

openInput :: (MonadIO m, MonadError String m) => InputSource -> m AVFormatContext
openInput ipt =
  case ipt of
    File fileName -> openFile fileName
    Camera cam cf -> openCamera cam cf

-- | Open an input media file.
openFile :: (MonadIO m, MonadError String m) => String -> m AVFormatContext
openFile filename =
  wrapIOError . alloca $ \ctx ->
    withCString filename $ \cstr ->
      do poke (castPtr ctx) nullPtr
         r <- avformat_open_input ctx cstr nullPtr nullPtr
         when (r /= 0) (stringError r >>= \s ->
                          fail $ "ffmpeg failed opening file: " ++ s)
         peek ctx

-- | @AVFrame@ is a superset of @AVPicture@, so we can upcast an
-- 'AVFrame' to an 'AVPicture'.
frameAsPicture :: AVFrame -> AVPicture
frameAsPicture = AVPicture . getPtr

-- | Find a codec given by name.
findDecoder :: (MonadIO m, MonadError String m) => String -> m AVCodec
findDecoder name =
  do r <- liftIO $ withCString name avcodec_find_decoder_by_name
     when (getPtr r == nullPtr)
          (throwError $ "Unsupported codec: " ++ show name)
     return r

-- | Read packets of a media file to get stream information. This is
-- useful for file formats with no headers such as MPEG.
checkStreams :: (MonadIO m, MonadError String m) => AVFormatContext -> m ()
checkStreams ctx =
  do r <- liftIO $ avformat_find_stream_info ctx nullPtr
     when (r < 0) (throwError "Couldn't find stream information")

-- | Searches for a video stream in an 'AVFormatContext'. If one is
-- found, returns the index of the stream in the container, and its
-- associated 'AVCodecContext' and 'AVCodec'.
findVideoStream :: (MonadIO m, MonadError String m)
                => AVFormatContext
                -> m (CInt, AVCodecContext, AVCodec, AVStream)
findVideoStream fmt = do
  wrapIOError . alloca $ \codec -> do
      poke codec (AVCodec nullPtr)
      i <- av_find_best_stream fmt avmediaTypeVideo (-1) (-1) codec 0
      when (i < 0) (fail "Couldn't find a video stream")
      cod <- peek codec
      streams <- getStreams fmt
      vidStream <- peek (advancePtr streams (fromIntegral i))
      ctx <- getCodecContext vidStream
      return (i, ctx, cod, vidStream)

findAudioStream :: (MonadIO m, MonadError String m)
                => AVFormatContext
                -> m (CInt, AVCodecContext, AVCodec, AVStream)
findAudioStream fmt = do
  wrapIOError . alloca $ \codec -> do
      poke codec (AVCodec nullPtr)
      i <- av_find_best_stream fmt avmediaTypeAudio (-1) (-1) codec 0
      when (i < 0) (fail "Couldn't find audio stream")
      cod <- peek codec
      streams <- getStreams fmt
      audioStream <- peek (advancePtr streams (fromIntegral i))
      ctx <- getCodecContext audioStream
      return (i, ctx, cod, audioStream)

-- | Find a registered decoder with a codec ID matching that found in
-- the given 'AVCodecContext'.
getDecoder :: (MonadIO m, MonadError String m)
           => AVCodecContext -> m AVCodec
getDecoder ctx = do p <- liftIO $ getCodecID ctx >>= avcodec_find_decoder
                    when (getPtr p == nullPtr) (throwError "Unsupported codec")
                    return p

-- | Initialize the given 'AVCodecContext' to use the given
-- 'AVCodec'. **NOTE**: This function is not thread safe!
openCodec :: (MonadIO m, MonadError String m)
          => AVCodecContext -> AVCodec -> m AVDictionary
openCodec ctx cod =
  wrapIOError . alloca $ \dict -> do
    poke dict (AVDictionary nullPtr)
    r <- open_codec ctx cod dict
    when (r < 0) (fail "Couldn't open decoder")
    peek dict

-- | Return the next frame of a stream.
read_frame_check :: AVFormatContext -> AVPacket -> IO ()
read_frame_check ctx pkt = do r <- av_read_frame ctx pkt
                              when (r < 0) (fail "Frame read failed")

-- | Read frames of the given 'AVPixelFormat' from a video stream.
frameReader :: (MonadIO m, MonadError String m)
            => AVPixelFormat -> InputSource -> m (IO (Maybe AVFrame), IO ())
frameReader dstFmt ipt =
  do inputContext <- openInput ipt
     checkStreams inputContext
     (vidStreamIndex, ctx, cod, _vidStream) <- findVideoStream inputContext
     _ <- openCodec ctx cod
     prepareReader inputContext vidStreamIndex dstFmt ctx

-- | Read RGB frames with the result in the 'MaybeT' transformer.
--
-- > frameReaderT = fmap (first MaybeT) . frameReader
frameReaderT :: (Functor m, MonadIO m, MonadError String m)
             => InputSource -> m (MaybeT IO AVFrame, IO ())
frameReaderT = fmap (first MaybeT) . frameReader avPixFmtRgb24

-- | Read time stamped frames of the given 'AVPixelFormat' from a
-- video stream. Time is given in seconds from the start of the
-- stream.
frameReaderTime :: (MonadIO m, MonadError String m)
                => AVPixelFormat -> InputSource
                -> m (IO (Maybe (AVFrame, Double)), IO ())
frameReaderTime dstFmt src =
  do inputContext <- openInput src
     checkStreams inputContext
     (vidStreamIndex, ctx, cod, vidStream) <- findVideoStream inputContext
     _ <- openCodec ctx cod
     (reader, cleanup) <- prepareReader inputContext vidStreamIndex dstFmt ctx
     AVRational num den <- liftIO $ getTimeBase vidStream
     let (numl, dend) = (fromIntegral num, fromIntegral den)
         frameTime' frame =
           do n <- getPts frame
              return $ fromIntegral (n * numl) / dend
         readTS = do frame <- reader
                     case frame of
                       Nothing -> return Nothing
                       Just f -> do t <- frameTime' f
                                    return $ Just (f, t)
     return (readTS, cleanup)

frameAudioReader :: (MonadIO m, MonadError String m)
                 => InputSource -> m (AudioStream, IO (Maybe AVFrame), IO ())
frameAudioReader fileName = do
  inputContext <- openInput fileName
  checkStreams inputContext
  (audioStreamIndex, ctx, cod, audioStream) <- findAudioStream inputContext
  openCodec ctx cod
  as <- liftIO $ do
    bitrate <- getBitRate ctx
    samplerate <- getSampleRate ctx
    channelLayout <- getChannelLayout ctx
    sampleFormat <- getSampleFormat ctx
    channels <- getChannels ctx
    codecId <- getCodecID cod
    return $ AudioStream
        { asBitRate = bitrate
        , asSampleRate = samplerate
        , asSampleFormat = sampleFormat
        , asChannelLayout = channelLayout
        , asChannelCount = channels
        , asCodec = codecId
        }
  (readFrame, finalize) <- prepareAudioReader inputContext audioStreamIndex ctx
  return (as, readFrame, finalize)

-- | Read time stamped RGB frames with the result in the 'MaybeT'
-- transformer.
--
-- > frameReaderT = fmap (first MaybeT) . frameReader
frameReaderTimeT :: (Functor m, MonadIO m, MonadError String m)
                 => InputSource -> m (MaybeT IO (AVFrame, Double), IO ())
frameReaderTimeT = fmap (first MaybeT) . frameReaderTime avPixFmtRgb24

prepareAudioReader :: (MonadIO m, MonadError String m)
                   => AVFormatContext -> CInt -> AVCodecContext
                   -> m (IO (Maybe AVFrame), IO ())
prepareAudioReader fmtCtx audStream codCtx =
  wrapIOError $ do
    frame <- frame_alloc_check
    pkt <- av_packet_alloc
    let cleanup = do with frame av_frame_free
                     _ <- codec_close codCtx
                     with fmtCtx close_input
                     free (getPtr pkt)
        getFrame = do
         r <- av_read_frame fmtCtx pkt
         if r < 0 then return Nothing else do
          whichStream <- getStreamIndex pkt
          if whichStream == audStream
          then do
            let decode_func = do
                  (got_frame, len) <- alloca $ \gt -> do
                    r <- runWithError "Error coding audio frame" $
                             decode_audio codCtx frame gt pkt
                    return (gt, r)
                  gf <- peek got_frame
                  if gf == 1
                    then return ()
                    else do
                      sz <- getSize pkt
                      dt <- getPktData pkt
                      setSize pkt (sz-len)
                      setPktData pkt (advancePtr dt (fromIntegral len))
                      endSize <- getSize pkt
                      if endSize <= 0 then return () else decode_func
            decode_func
            free_packet pkt
            return (Just frame)
          else free_packet pkt >> getFrame
    return (getFrame `catchError` const (return Nothing), cleanup)

-- | Construct an action that gets the next available frame, and an
-- action to release all resources associated with this video stream.
prepareReader :: (MonadIO m, MonadError String m)
              => AVFormatContext -> CInt -> AVPixelFormat -> AVCodecContext
              -> m (IO (Maybe AVFrame), IO ())
prepareReader fmtCtx vidStream dstFmt codCtx =
  wrapIOError $
  do fRaw <- frame_alloc_check
     fRgb <- frame_alloc_check

     w <- getWidth codCtx
     h <- getHeight codCtx
     fmt <- getPixelFormat codCtx

     setWidth fRgb w
     setHeight fRgb h
     setPixelFormat fRgb dstFmt

     frame_get_buffer_check fRgb 32

     sws <- swsInit (ImageInfo w h fmt) (ImageInfo w h dstFmt) swsBilinear

     pkt <- AVPacket <$> mallocBytes packetSize
     let cleanup = do with fRgb av_frame_free
                      with fRaw av_frame_free
                      _ <- codec_close codCtx
                      with fmtCtx close_input
                      free (getPtr pkt)
         getFrame = do
           read_frame_check fmtCtx pkt
           whichStream <- getStreamIndex pkt
           if whichStream == vidStream
           then do
             fin <- alloca $ \finished -> do
                      _ <- decode_video codCtx fRaw finished pkt
                      peek finished
             if fin > 0
             then do
               -- Some streaming codecs require a final flush with
               -- an empty packet
               -- fin' <- alloca $ \fin2 -> do
               --           free_packet pkt
               --           (#poke AVPacket, data) pkt nullPtr
               --           (#poke AVPacket, size) pkt (0::CInt)
               --           decode_video codCtx fRaw fin2 pkt
               --           peek fin2

               _ <- swsScale sws fRaw fRgb

               -- Copy the raw frame's timestamp to the RGB frame
               getPktPts fRaw >>= setPts fRgb

               free_packet pkt
               return $ Just fRgb
             else free_packet pkt >> getFrame
           else free_packet pkt >> getFrame
     return (getFrame `catchError` const (return Nothing), cleanup)
