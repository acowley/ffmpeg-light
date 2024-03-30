{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE TupleSections            #-}
-- | Video decoding API. Includes FFI declarations for the underlying
-- FFmpeg functions, wrappers for these functions that wrap error
-- condition checking, and high level Haskellized interfaces.
module Codec.FFmpeg.Decode where

import           Codec.FFmpeg.AudioStream
import           Codec.FFmpeg.Common
import           Codec.FFmpeg.Enums
import           Codec.FFmpeg.Scaler
import           Codec.FFmpeg.Types
import           Codec.FFmpeg.Display
import           Control.Monad.Except
import           Control.Monad.IO.Class    (MonadIO(liftIO))
import           Control.Monad.Trans.Maybe
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc     (alloca, free, mallocBytes)
import           Foreign.Marshal.Array     (advancePtr, peekArray)
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

foreign import ccall "av_image_get_buffer_size"
  av_image_get_buffer_size :: AVPixelFormat -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall "av_malloc"
  av_malloc :: CSize -> IO (Ptr ())

foreign import ccall "av_read_frame"
  av_read_frame :: AVFormatContext -> AVPacket -> IO CInt

foreign import ccall "avcodec_send_frame"
  decode_video :: AVCodecContext -> AVFrame -> IO CInt
  
foreign import ccall "avformat_close_input"
  close_input :: Ptr AVFormatContext -> IO ()

foreign import ccall "av_dict_set"
  av_dict_set :: Ptr AVDictionary -> CString -> CString -> CInt -> IO CInt

foreign import ccall "av_find_input_format"
  av_find_input_format :: CString -> IO (Ptr AVInputFormat)

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
         setUrl avfc c
         when (format cfg == Just "mjpeg") $ do
           mjpeg <- avcodec_find_decoder avCodecIdMjpeg
           setVideoCodecID avfc avCodecIdMjpeg
           setVideoCodec avfc mjpeg

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
      ctx <- createCodecContext cod vidStream
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
      ctx <- createCodecContext cod audioStream
      return (i, ctx, cod, audioStream)

createCodecContext :: HasCodecParams t => AVCodec -> t -> IO AVCodecContext
createCodecContext cod stream = do
  pCodecCtx <- avcodec_alloc_context3 cod;
  codecPar <- getCodecParams stream
  res <- avcodec_parameters_to_context pCodecCtx codecPar
  when (res < 0) (fail "Couldn't get codec parameters for video stream")
  pure pCodecCtx
      
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
readFrameCheck :: AVFormatContext -> AVPacket -> IO ()
readFrameCheck ctx pkt = do 
  r <- av_read_frame ctx pkt
  when (r < 0) (fail "Frame read failed")

data VideoStreamMetadata = VideoStreamMetadata
  { metadata :: Maybe AVDictionary
  , displayRotation :: Maybe DisplayRotationDegrees
  }

extractVideoStreamMetadata :: MonadIO m => AVStream -> m VideoStreamMetadata
extractVideoStreamMetadata vidStream = do
  sd <- getStreamSideData vidStream
  VideoStreamMetadata <$> structMetadata vidStream <*> extractDisplayRotation sd

-- | Read frames of the given 'AVPixelFormat' from a video stream.
-- | Also read side data or metadata of stream and return this if present
frameReader :: (MonadIO m, MonadError String m)
            => AVPixelFormat -> InputSource -> m (IO (Maybe AVFrame), IO (), VideoStreamMetadata )
frameReader dstFmt ipt = do 
  inputContext <- openInput ipt
  checkStreams inputContext
  (vidStreamIndex, ctx, cod, vidStream) <- findVideoStream inputContext
  _ <- openCodec ctx cod
  metaAndSide <- extractVideoStreamMetadata vidStream
  (reader, cleanup) <- prepareReader inputContext vidStreamIndex dstFmt ctx
  pure (reader, cleanup, metaAndSide)

-- | Read RGB frames with the result in the 'MaybeT' transformer.
--
-- > frameReaderT = fmap (first MaybeT) . frameReader
frameReaderT :: (Functor m, MonadIO m, MonadError String m)
             => InputSource -> m (MaybeT IO AVFrame, IO (), VideoStreamMetadata)
frameReaderT = fmap (first3 MaybeT) . frameReader avPixFmtRgb24
  
-- | Read time stamped frames of the given 'AVPixelFormat' from a
-- video stream. Time is given in seconds from the start of the
-- stream.
frameReaderTime :: (MonadIO m, MonadError String m)
                => AVPixelFormat -> InputSource
                -> m (IO (Maybe (AVFrame, Double)), IO (), VideoStreamMetadata)
frameReaderTime dstFmt src =
  do inputContext <- openInput src
     checkStreams inputContext
     (vidStreamIndex, ctx, cod, vidStream) <- findVideoStream inputContext
     metaAndSide <- extractVideoStreamMetadata vidStream
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
     return (readTS, cleanup, metaAndSide)

frameAudioReader :: (MonadIO m, MonadError String m)
                 => InputSource -> m (AudioStream, IO (Maybe AVFrame), IO (), Maybe AVDictionary)
frameAudioReader fileName = do
  inputContext <- openInput fileName
  checkStreams inputContext
  (audioStreamIndex, ctx, cod, audioStream) <- findAudioStream inputContext
  metadata <- structMetadata audioStream
  void (openCodec ctx cod)
  as <- liftIO $ do
    bitrate <- getBitRate ctx
    samplerate <- getSampleRate ctx
    channelLayout <- getChannelLayout ctx
    sampleFormat <- getSampleFormat ctx
    let channels = numChannels channelLayout
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
  return (as, readFrame, finalize, metadata)

-- | Read time stamped RGB frames with the result in the 'MaybeT'
-- transformer.
--
-- > frameReaderT = fmap (first MaybeT) . frameReader
frameReaderTimeT :: (Functor m, MonadIO m, MonadError String m)
                 => InputSource -> m (MaybeT IO (AVFrame, Double), IO (), VideoStreamMetadata)
frameReaderTimeT = fmap (first3 MaybeT) . frameReaderTime avPixFmtRgb24

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
          ret <- avcodec_receive_frame codCtx frame
          if ret < 0
            then do
              r <- av_read_frame fmtCtx pkt
              if r < 0
                then return Nothing
                else do
                  whichStream <- getStreamIndex pkt
                  if whichStream == audStream
                    then do
                      runWithError "Error sending packet" (avcodec_send_packet codCtx pkt)
                      getFrame
                    else free_packet pkt >> getFrame
            else return $ Just frame
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
         -- This function follows the steps from https://ffmpeg.org/doxygen/trunk/group__lavc__encdec.html
         getFrame = do
            recvdFrame  <- avcodec_receive_frame codCtx fRaw
            if recvdFrame == c_AVERROR_EAGAIN then do
              readFrameCheck fmtCtx pkt
              whichStream <- getStreamIndex pkt
              if whichStream == vidStream then do
                avcodec_send_packet codCtx pkt -- TODO: non zero is an error here
                free_packet pkt
                getFrame
             
               -- Some streaming codecs require a final flush with
               -- an empty packet
               -- fin' <- alloca $ \fin2 -> do
               --           free_packet pkt
               --           (#poke AVPacket, data) pkt nullPtr
               --           (#poke AVPacket, size) pkt (0::CInt)
               --           decode_video codCtx fRaw fin2 pkt
               --           peek fin2
              else free_packet pkt >> getFrame           
            else do
              _ <- swsScale sws fRaw fRgb

              -- Copy the raw frame's timestamp to the RGB frame
              getPktDts fRaw >>= setPts fRgb

              free_packet pkt
              return $ Just fRgb
           
     return (getFrame `catchError` const (return Nothing), cleanup)

getStreamSideData :: MonadIO m => AVStream -> m [AVPacketSideData]
getStreamSideData avstream = liftIO $ do
  nbs <- fromIntegral <$> getNbSideData avstream
  if nbs > 0 then do
    sdArray <- getSideData avstream
    peekArray nbs sdArray
  else  pure []

extractDisplayRotation :: MonadIO m => [AVPacketSideData] -> m (Maybe DisplayRotationDegrees)
extractDisplayRotation lst = go Nothing lst
  where 
    go dsp@(Just _) _ = pure dsp
    go Nothing (nextElem:xs) = do 
      disprm <- liftIO (getDisplayRotation nextElem)
      case disprm of 
        Nothing -> go Nothing xs
        Just dispr -> pure (Just dispr)
    go anything [] = pure anything
    


