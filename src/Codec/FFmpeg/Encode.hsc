{-# LANGUAGE ForeignFunctionInterface #-}
-- | Video encoding API. Includes FFI declarations for the underlying
-- FFmpeg functions, wrappers for these functions that wrap error
-- condition checking, and high level Haskellized interfaces.
--
-- Note: If you need to import this module, consider qualifying the
-- import.
module Codec.FFmpeg.Encode where
import Codec.FFmpeg.Common
import Codec.FFmpeg.Enums
import Codec.FFmpeg.Internal.Linear
import Codec.FFmpeg.Resampler
import Codec.FFmpeg.Scaler
import Codec.FFmpeg.Types
import Codec.Picture
import Control.Monad (when, void)
import Data.Bits
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Traversable (for)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array (advancePtr)
import Foreign.Marshal.Utils

import Codec.FFmpeg.Internal.Debug

import Foreign.Ptr
import Foreign.Storable

#include <libavformat/avformat.h>

-- Based on the FFmpeg muxing example
-- http://www.ffmpeg.org/doxygen/2.1/doc_2examples_2muxing_8c-example.html

-- * FFI Declarations

foreign import ccall "avcodec_find_encoder"
  avcodec_find_encoder :: AVCodecID -> IO AVCodec

foreign import ccall "avcodec_find_encoder_by_name"
  avcodec_find_encoder_by_name :: CString -> IO AVCodec

foreign import ccall "av_opt_set"
  av_opt_set :: Ptr () -> CString -> CString -> CInt -> IO CInt

foreign import ccall "avcodec_encode_video2"
  avcodec_encode_video2 :: AVCodecContext -> AVPacket -> AVFrame -> Ptr CInt
                        -> IO CInt

foreign import ccall "av_image_alloc"
  av_image_alloc :: Ptr (Ptr CUChar) -> Ptr CInt -> CInt -> CInt
                 -> AVPixelFormat -> CInt -> IO CInt

foreign import ccall "av_freep"
  av_freep :: Ptr (Ptr a) -> IO ()

foreign import ccall "av_guess_format"
  av_guess_format :: CString -> CString -> CString -> IO AVOutputFormat

foreign import ccall "avformat_alloc_output_context2"
  avformat_alloc_output_context2 :: Ptr AVFormatContext -> AVOutputFormat
                                 -> CString -> CString -> IO CInt

foreign import ccall "avformat_new_stream"
  avformat_new_stream :: AVFormatContext -> AVCodec -> IO AVStream

foreign import ccall "av_write_frame"
  av_write_frame :: AVFormatContext -> AVPacket -> IO CInt

foreign import ccall "av_interleaved_write_frame"
  av_interleaved_write_frame :: AVFormatContext -> AVPacket -> IO CInt

foreign import ccall "avformat_write_header"
  avformat_write_header :: AVFormatContext -> Ptr AVDictionary -> IO CInt

foreign import ccall "av_write_trailer"
  av_write_trailer :: AVFormatContext -> IO CInt

foreign import ccall "avio_open"
  avio_open :: Ptr AVIOContext -> CString -> AVIOFlag -> IO CInt

foreign import ccall "avio_close"
  avio_close :: AVIOContext -> IO CInt

foreign import ccall "avformat_free_context"
  avformat_free_context :: AVFormatContext -> IO ()

foreign import ccall "av_image_fill_arrays"
  av_image_fill_arrays :: Ptr (Ptr CUChar) -> Ptr CInt -> Ptr CUChar
                       -> AVPixelFormat -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall "av_image_fill_linesizes"
  av_image_fill_linesizes :: Ptr CInt -> AVPixelFormat -> CInt -> IO CInt

foreign import ccall "av_frame_make_writable"
  av_frame_make_writable :: AVFrame -> IO CInt

-- * FFmpeg Encoding Interface

-- | Minimal parameters describing the desired media output.
data EncodingParams = EncodingParams
  { epCodec  :: Maybe AVCodecID
  -- ^ If 'Nothing', then the codec is inferred from
  -- the output file name. If 'Just', then this codec
  -- is manually chosen.
  , epFormatName :: Maybe String
  -- ^ FFmpeg muxer format name. If 'Nothing', tries to infer
  -- from the output file name. If 'Just', the string value
  -- should be the one available in @ffmpeg -formats@.
  , epStreamParams :: StreamParams
  -- ^ Encoding parameters for video, audio, or both
  }

data StreamParams =
    JustVideo VideoParams
  | JustAudio AudioParams
  | AudioVideo AudioParams VideoParams

data VideoParams = VideoParams
  { vpWidth  :: CInt
  , vpHeight :: CInt
  , vpFps    :: Int
  , vpPixelFormat :: Maybe AVPixelFormat
  -- ^ If 'Nothing', automatically chose a pixel format
  -- based on the output codec. If 'Just', force the
  -- selected pixel format.
  , vpPreset :: String
  -- ^ Encoder-specific hints. For h264, the default
  -- preset is @\"medium\"@ (other options are
  -- @\"fast\"@, @\"slow\"@, etc.). For the GIF codec,
  -- setting this to @\"dither\"@ will enable dithering
  -- during the palettization process. This will
  -- improve image quality, but result in a larger
  -- file.
  }

withVideoParams :: EncodingParams -> a -> (VideoParams -> a) -> a
withVideoParams ep def f = withVPs (epStreamParams ep)
  where
    withVPs (JustVideo vp) = f vp
    withVPs (AudioVideo _ vp) = f vp
    withVPs _ = def

withAudioParams :: EncodingParams -> a -> (AudioParams -> a) -> a
withAudioParams ep def f = withVPs (epStreamParams ep)
  where
    withVPs (JustAudio ap) = f ap
    withVPs (AudioVideo ap _) = f ap
    withVPs _ = def

-- | Use default parameters for a video of the given width and
-- height, forcing the choice of the h264 encoder.
defaultH264 :: CInt -> CInt -> EncodingParams
defaultH264 w h =
  EncodingParams
    { epCodec = (Just avCodecIdH264)
    , epFormatName = Nothing
    , epStreamParams = JustVideo (VideoParams w h 30 Nothing "medium")
    }

-- | Use default parameters for a video of the given width and
-- height. The output format is determined by the output file name.
defaultParams :: CInt -> CInt -> EncodingParams
defaultParams w h =
  EncodingParams
    { epCodec = Nothing
    , epFormatName = Nothing
    , epStreamParams = JustVideo (VideoParams w h 30 Nothing "")
    }

-- | Determine if the bitwise intersection of two values is non-zero.
checkFlag :: Bits a => a -> a -> Bool
checkFlag flg = \x -> (flg .&. x) /= allZeroBits
  where allZeroBits = clearBit (bit 0) 0

-- | Find and initialize the requested encoder, and add a video stream
-- to the output container.
initStream :: Maybe AVCodecID -> VideoParams -> AVFormatContext -> IO (AVStream, AVCodecContext)
initStream _ vp _
  | (vpWidth vp `rem` 2, vpHeight vp `rem` 2) /= (0,0) =
    error "Video dimensions must be multiples of two"
initStream codecId vp oc = do
  -- Use the codec suggested by the output format, or override with
  -- the user's choice.
  codec <- maybe (getOutputFormat oc >>= getVideoCodecID) return codecId
  cod <- avcodec_find_encoder codec
  when (getPtr cod == nullPtr)
       (error "Couldn't find encoder")

  st <- avformat_new_stream oc cod
  getNumStreams oc >>= setId st . subtract 1
  let framePeriod = AVRational 1 (fromIntegral $ vpFps vp)
      frameRate = AVRational (fromIntegral $ vpFps vp) 1
  setTimeBase st framePeriod
  ctx <- getCodecContext st
  setWidth ctx (vpWidth vp)
  setHeight ctx (vpHeight vp)
  setTimeBase ctx framePeriod
  setFrameRate ctx frameRate
  setPixelFormat ctx $ case vpPixelFormat vp of
                         Just fmt -> fmt
                         Nothing
                           | codec == avCodecIdRawvideo -> avPixFmtRgb24
                           | codec == avCodecIdGif -> avPixFmtPal8
                           | otherwise -> avPixFmtYuv420p

  -- Some formats want stream headers to be separate
  needsHeader <- checkFlag avfmtGlobalheader <$>
                 (getOutputFormat oc >>= getFormatFlags)
  when needsHeader $
#if LIBAVFORMAT_VERSION_MAJOR < 57
    getCodecFlags ctx >>= setCodecFlags ctx . (.|. codecFlagGlobalHeader)
#else
    getCodecFlags ctx >>= setCodecFlags ctx . (.|. avCodecFlagGlobalHeader)
#endif

  -- _ <- withCString "vprofile" $ \kStr ->
  --        withCString (preset ep) $ \vStr ->
  --          av_opt_set ((#ptr AVCodecContext, priv_data) (getPtr ctx))
  --                     kStr vStr 0
  when (not . null $ vpPreset vp) . void $
    withCString "preset" $ \kStr ->
      withCString (vpPreset vp) $ \vStr ->
        getPrivData ctx >>= \pd -> av_opt_set pd kStr vStr 0

  rOpen <- open_codec ctx cod nullPtr
  when (rOpen < 0) (error "Couldn't open codec")

  return (st, ctx)

initAudioStream :: AudioParams
                -> AVFormatContext
                -> IO (AVStream, AVCodec, AVCodecContext)
initAudioStream opts oc = do
  codecId <- getAudioCodecID =<< getOutputFormat oc
  cod <- avcodec_find_encoder codecId
  when (getPtr cod == nullPtr) (avError "Could not find audio codec")

  st <- avformat_new_stream oc cod
  getNumStreams oc >>= setId st . subtract 1
  setTimeBase st (AVRational 1 (apSampleRate opts))

  ctx <- avcodec_alloc_context3 cod

  supportedSampleRates <- listSupportedSampleFormats cod
  let found = not (null supportedSampleRates)
  when (not found) $ avError "Could not find supported sample rate"
  -- TODO: check that these are valid
  setSampleFormat ctx (apSampleFormat opts)
  setSampleRate ctx (apSampleRate opts)

  setChannelLayout ctx (apChannelLayout opts)

  runWithError "Could not open audio codec" (open_codec ctx cod nullPtr)

  codecParams <- getCodecParams st
  runWithError "Could not copy params" (avcodec_parameters_from_context codecParams ctx)

  return (st, cod, ctx)


-- | Initialize a temporary YUV frame of the same resolution as the
-- output video stream. We well convert RGB frames using this frame as
-- a destination before encoding the video frame.
initTempFrame :: VideoParams -> AVPixelFormat -> IO AVFrame
initTempFrame vp fmt = do
  frame <- frame_alloc_check
  setPixelFormat frame fmt
  setWidth frame (vpWidth vp)
  setHeight frame (vpHeight vp)
  setPts frame 0

  -- For palettized images, we will provide our own buffer.
  if fmt == avPixFmtRgb8 || fmt == avPixFmtPal8
  then do r <- av_image_fill_linesizes (hasLineSize frame) fmt (vpWidth vp)
          when (r < 0) (error "Error filling temporary frame line sizes")
  else frame_get_buffer_check frame 32
  return frame

-- | Allocate an output context inferring the codec from the given
-- file name.
allocOutputContext :: Maybe String -> FilePath -> IO AVFormatContext
allocOutputContext outputFormat fname = do
  let withFormat = case outputFormat of
        Just f -> withCString f
        Nothing -> (\f -> f nullPtr)
  oc <- alloca $ \ocTmp -> do
        r <- withCString fname $ \fname' ->
                withFormat $ \format ->
                  avformat_alloc_output_context2
                    ocTmp (AVOutputFormat nullPtr)
                    format fname'
        when (r < 0)
              (error "Couldn't allocate output format context")
        peek ocTmp
  when (getPtr oc == nullPtr)
       (error "Couldn't allocate iutput AVFormatContext")
  return oc

-- | Open the given file for writing.
avio_open_check :: AVFormatContext -> String -> IO ()
avio_open_check oc fname =
  do r <- withCString fname $ \cstr ->
            avio_open (hasIOContext oc) cstr avioFlagWrite
     when (r < 0) (error "Error opening IO for writing")

-- | Close an open IO context.
avio_close_check :: AVFormatContext -> IO ()
avio_close_check oc = do r <- getIOContext oc >>= avio_close
                         when (r /= 0) (error "Error closing IO")

-- | Returns 'True' if the 'AVPacket' was updated with new output
-- data; 'False' otherwise.
encode_video_check :: AVCodecContext -> AVPacket -> Maybe AVFrame -> IO Bool
encode_video_check ctx pkt frame =
  alloca $ \gotOutput -> do
    r <- avcodec_encode_video2 ctx pkt frame' gotOutput
    when (r < 0) (error "Error encoding frame")
    (> 0) <$> peek gotOutput
  where frame' = fromMaybe (AVFrame nullPtr) frame

-- | Allocate the stream private data and write the stream header to
-- an output media file.
write_header_check :: AVFormatContext -> IO ()
write_header_check oc =
  void $ runWithError "write header" (avformat_write_header oc nullPtr)

-- | Write a packet to an output media file.
write_frame_check :: AVFormatContext -> AVPacket -> IO ()
write_frame_check oc pkt = do r <- av_write_frame oc pkt
                              when (r < 0) (error "Error writing frame")

-- | Write the stream trailer to an output media file and free the
-- private data. May only be called after a successful call to
-- 'write_header_check'.
write_trailer_check :: AVFormatContext -> IO ()
write_trailer_check oc = do r <- av_write_trailer oc
                            when (r /= 0) (error "Error writing trailer")

-- | Quantize RGB24 pixels to the systematic RGB8 color palette. The
-- image data has space for a palette appended to be compliant with
-- 'av_image_fill_arrays''s expectations. This is slow.
palettizeRGB8 :: VideoParams -> V.Vector CUChar -> V.Vector CUChar
palettizeRGB8 vp = \pix -> V.create $
  do let pix' = V.unsafeCast pix :: V.Vector (V3 CUChar)
     m <- VM.new (numPix + 1024)
     V.mapM_ (\i -> let p = searchPal $ fromIntegral <$> (pix' V.! i)
                    in VM.unsafeWrite m i p)
             (V.enumFromN 0 numPix)
     VM.set (VM.unsafeSlice numPix 1024 m) 0
     return m
  where numPix = fromIntegral $ vpWidth vp * vpHeight vp
        pal :: V.Vector (V3 CInt)
        pal = V.generate 256 $ \i' ->
                let i = fromIntegral i'
                in V3 ((i `shiftR` 5) * 36)
                      (((i `shiftR` 2) .&. 7) * 36)
                      ((i .&. 3) * 85)
        searchPal = fromIntegral . flip V.minIndexBy pal . comparing . qd

-- | High quality dithered, median cut palette using 'palettize'. The
-- result is packed such that the BGRA palette is laid out
-- contiguously following the palettized image data.
palettizeJuicy :: VideoParams -> V.Vector CUChar -> V.Vector CUChar
palettizeJuicy vp pix =
  let (pix', pal) = palettize (PaletteOptions MedianMeanCut doDither 256)
                              (mkImage $ V.unsafeCast pix)
      pal' = V.map (\(V3 r g b) -> V4 b g r (255::CUChar))
                  (V.unsafeCast $ imageData pal)
  in V.unsafeCast (imageData pix') V.++ V.unsafeCast pal'
  where mkImage = Image (fromIntegral $ vpWidth vp) (fromIntegral $ vpHeight vp)
        doDither = vpPreset vp == "dither"

-- | Open a target file for writing a video stream. The function
-- returned may be used to write image frames (specified by a pixel
-- format, resolution, and pixel data). If this function is applied to
-- 'Nothing', then the output stream is closed. Note that 'Nothing'
-- /must/ be provided to properly terminate video encoding.
--
-- Support for source images that are of a different size to the
-- output resolution is limited to non-palettized destination formats
-- (i.e. those that are handled by @libswscaler@). Practically, this
-- means that animated gif output only works if the source images are
-- of the target resolution.
frameWriter :: EncodingParams -> FilePath
            -> IO ( Maybe AVCodecContext
                  , Maybe AVCodecContext
                  , Maybe (AVPixelFormat, V2 CInt, Vector CUChar) -> IO ()
                  , Maybe AVFrame -> IO ()
                  )
frameWriter ep fname = do
  let outputFormat = epFormatName ep
  oc <- allocOutputContext outputFormat fname
  outputFormat <- getOutputFormat oc
  audioCodecId <- getAudioCodecID outputFormat
  videoCodecId <- getVideoCodecID outputFormat

  -- Initializing the streams needs to be done before opening the file
  -- and checking the header because it can modify fields that are used
  -- for time scaling so we have this rather ugly code.
  mVideoStream <- withVideoParams ep (return Nothing) $ \vp ->
                    (Just <$> initStream (epCodec ep) vp oc)
  mAudioStream <- withAudioParams ep (return Nothing) $ \ap ->
                    (Just <$> initAudioStream ap oc)
  avio_open_check oc fname
  numStreams <- getNumStreams oc
  write_header_check oc

  alreadyClosedRef <- newIORef False
  let writeClose = do
        alreadyClosed <- readIORef alreadyClosedRef
        when (not alreadyClosed) $ do
          write_trailer_check oc
          modifyIORef alreadyClosedRef (const True)

      initializeVideo :: AVStream
                      -> AVCodecContext
                      -> VideoParams
                      -> IO (Maybe (AVPixelFormat, V2 CInt, Vector CUChar) -> IO ())
      initializeVideo st ctx vp = do
        dstFmt <- getPixelFormat ctx
        dstFrame <- initTempFrame vp dstFmt
        let dstInfo = ImageInfo (vpWidth vp) (vpHeight vp) dstFmt

        -- Initialize the scaler that we use to convert RGB -> dstFmt
        -- Note that libswscaler does not support Pal8 as an output format.
        sws <- if dstFmt /= avPixFmtPal8 && dstFmt /= avPixFmtRgb8
              then swsInit (ImageInfo (vpWidth vp) (vpHeight vp) avPixFmtRgb24)
                            dstInfo swsBilinear
                    >>= fmap Just . newIORef
              else return Nothing

        pkt <- AVPacket <$> av_malloc (fromIntegral packetSize)
        setPts pkt 0

        stIndex <- getStreamIndex st

        -- Frame number ioref. We use this to determine whether we should
        -- increment the frame PTS; we only want to do this for frames after
        -- the first one since we want the first frame PTS to be zero.
        frameNum <- newIORef (0::Int)

        let framePeriod = AVRational 1 (fromIntegral $ vpFps vp)
        fps <- getFps ctx

          -- The stream time_base can be changed by the call to
          -- 'write_header_check', so we read it back here to establish a way
          -- of scaling the nominal, desired frame rate (given by
          -- 'framePeriod') to the stream's time_base.
        tb <- getTimeBase st
#if LIBAVFORMAT_VERSION_MAJOR < 57
        isRaw <- checkFlag avfmtRawpicture <$> (getOutputFormat oc >>= getFormatFlags)
#endif

        let checkPalCompat
              | dstFmt /= avPixFmtPal8 && dstFmt /= avPixFmtRgb8 = const True
              | otherwise = \(srcFmt, V2 srcW srcH, _) ->
                              srcFmt == avPixFmtRgb24 &&
                              srcW == vpWidth vp &&
                              srcH == vpHeight vp

            palettizer | dstFmt == avPixFmtPal8 = Just $ palettizeJuicy vp
                       | dstFmt == avPixFmtRgb8 = Just $ palettizeRGB8 vp
                       | otherwise =  Nothing
            frameTime = av_rescale_q 1 framePeriod tb
            resetPacket = do init_packet pkt
                             setPktData pkt nullPtr
                             setSize pkt 0
            writePacket = do setStreamIndex pkt stIndex
                             write_frame_check oc pkt

            copyDstData (_,_,pixels) =
              void . V.unsafeWith pixels $ \ptr ->
                av_image_fill_arrays (castPtr $ hasData dstFrame)
                                    (hasLineSize dstFrame)
                                    (castPtr ptr)
                                    dstFmt
                                    (vpWidth vp)
                                    (vpHeight vp)
                                    1

            scaleToDst sws' img = void $ swsScale sws' img dstFrame
            fillDst = maybe copyDstData scaleToDst

            -- | Gets the PTS to be used for the current frame by reading the
            -- PTS from dstFrame. If the current frame is the first frame
            -- (zero), the existing timestamp is left unmodified. Otherwise it
            -- is incremented by frameTime.
            --
            -- This also increments the current frame number stored in the
            -- frameNum IORef so the caller needn't worry about it.
            getCurrentFrameTimestamp = do
                curFrame <- readIORef frameNum
                ts <- case curFrame == 0 of
                    True -> getPts dstFrame
                    False -> (+ frameTime) <$> getPts dstFrame
                modifyIORef frameNum (+1)
                return ts
#if LIBAVFORMAT_VERSION_MAJOR < 57
            addRaw Nothing = return ()
            addRaw (Just (_, _, pixels)) =
              do resetPacket
                getPacketFlags pkt >>= setPacketFlags pkt . (.|. avPktFlagKey)
                --setSize pkt (fromIntegral $ V.length pixels)
                setSize pkt (fromIntegral pictureSize)
                timeStamp <- getCurrentFrameTimestamp
                setPts dstFrame timeStamp
                setPts pkt timeStamp
                -- getPts dstFrame >>= setDts pkt
                V.unsafeWith pixels $ \ptr -> do
                  setData pkt (castPtr ptr)
                  writePacket
#endif
            addEncoded Nothing = do resetPacket
                                    encode_video_check ctx pkt Nothing >>=
                                      flip when (writePacket >> addEncoded Nothing)
            addEncoded (Just srcImg@(srcFmt, V2 srcW srcH, pixels)) =
              do resetPacket
                 when (not $ checkPalCompat srcImg)
                      (error $
                      unlines [ "Palettized output requires source images to be the "
                              , "same resolution as the output video" ])
                 let pixels' = maybe pixels ($ V.unsafeCast pixels) palettizer
                 sws' <- for sws $ \sPtr -> do
                          s <- readIORef sPtr
                          s' <- swsReset s (ImageInfo srcW srcH srcFmt) dstInfo
                                          swsBilinear
                          writeIORef sPtr s'
                          return s'
                 timeStamp <- getCurrentFrameTimestamp
                 setPts dstFrame timeStamp
                 fillDst sws' (srcFmt, V2 srcW srcH, pixels')
                 encode_video_check ctx pkt (Just dstFrame) >>= flip when writePacket
                 -- Make sure the GC hasn't clobbered our palettized pixel data
                 let (fp,_,_) = V.unsafeToForeignPtr pixels'
                 touchForeignPtr fp
#if LIBAVFORMAT_VERSION_MAJOR < 57
            addFrame = if isRaw then addRaw else addEncoded
#else
            addFrame = addEncoded
#endif
            go Nothing = do addFrame Nothing
                            writeClose
                            _ <- codec_close ctx
                            with dstFrame av_frame_free
                            av_free (getPtr pkt)
                            avio_close_check oc
                            avformat_free_context oc
            go img@(Just _) = addFrame img
        return go

      initializeAudio :: AVStream
                      -> AVCodec
                      -> AVCodecContext
                      -> AudioParams
                      -> IO (Maybe AVFrame -> IO ())
      initializeAudio st codec ctx ap = do
        if audioCodecId /= avCodecIdNone
          then do
            pkt <- av_packet_alloc

            frameNum <- newIORef (0::Int)
            timeBase <- getTimeBase ctx

            let read_pkts = do
                  ret <- avcodec_receive_packet ctx pkt
                  -- TODO: Distinguish between temp and permanent errors with EAGAIN
                  if ret /= 0
                    then return ()
                    else do
                      timeBase2 <- getTimeBase st
                      packet_rescale_ts pkt timeBase timeBase2
                      setStreamIndex pkt =<< getStreamIndex st
                      runWithError "Error while writing audio frame"
                                  (av_interleaved_write_frame oc pkt)
                      return ()
                writeAudioFrame :: Maybe AVFrame -> IO ()
                writeAudioFrame Nothing = do
                  read_pkts
                  writeClose
                  codec_close ctx
                  return ()
                writeAudioFrame (Just frame) = writeAudioFrame' frame

                writeAudioFrame' :: AVFrame -> IO ()
                writeAudioFrame' frame = do
                  numSamples <- getNumSamples frame
                  sampleRate <- getSampleRate ctx

                  onGoingSampleCount <- readIORef frameNum
                  let samplesCount = av_rescale_q (fromIntegral onGoingSampleCount)
                                              (AVRational 1 sampleRate) timeBase
                  setPts frame samplesCount
                  modifyIORef frameNum (+ fromIntegral numSamples)

                  runWithError "Error encoding audio"
                      (avcodec_send_frame ctx frame)
                  read_pkts
            return writeAudioFrame
          else
            return $ \_ -> return ()

  videoWriter <- case mVideoStream of
                   Just (vs, ctx) ->
                     withVideoParams ep (return (\_ -> return ()))
                       (initializeVideo vs ctx)
                   Nothing -> return (\_ -> return ())
  audioWriter <- case mAudioStream of
                   Just (as, codec, ctx) ->
                     withAudioParams ep (return $ \_ -> return ())
                       (initializeAudio as codec ctx)
                   Nothing -> return $ \_ -> return ()

  return ( snd <$> mVideoStream
         , (\(_, _, ctx) -> ctx) <$> mAudioStream
         , videoWriter
         , audioWriter
         )

-- | Open a target file for writing a video stream. The function
-- returned may be used to write RGB images of the resolution given by
-- the provided 'EncodingParams' (i.e. the same resolution as the
-- output video). If this function is applied to 'Nothing', then the
-- output stream is closed. Note that 'Nothing' /must/ be provided to
-- properly terminate video encoding. Throws an error if you do not
-- provide a 'EncodingParams' without 'VideoParams'
frameWriterRgb :: EncodingParams -> FilePath
               -> IO (Maybe (Vector CUChar) -> IO ())
frameWriterRgb ep f =
  withVideoParams ep (avError "You must provide VideoParams") $ \vp -> do
    let aux pixels = (avPixFmtRgb24, V2 (vpWidth vp) (vpHeight vp), pixels)
    (_, _, videoWriter, _) <- frameWriter ep f
    return $ \pix -> videoWriter (aux <$> pix)
