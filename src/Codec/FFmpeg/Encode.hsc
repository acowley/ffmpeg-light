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
import Data.Maybe (fromMaybe, isNothing)
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

-- | Minimal parameters describing the desired video output.
data EncodingParams = EncodingParams
  { epWidth  :: CInt
  , epHeight :: CInt
  , epFps    :: Int
  , epCodec  :: Maybe AVCodecID
  -- ^ If 'Nothing', then the codec is inferred from
  -- the output file name. If 'Just', then this codec
  -- is manually chosen.
  , epPixelFormat :: Maybe AVPixelFormat
  -- ^ If 'Nothing', automatically chose a pixel format
  -- based on the output codec. If 'Just', force the
  -- selected pixel format.
  , epPreset :: String
  -- ^ Encoder-specific hints. For h264, the default
  -- preset is @\"medium\"@ (other options are
  -- @\"fast\"@, @\"slow\"@, etc.). For the GIF codec,
  -- setting this to @\"dither\"@ will enable dithering
  -- during the palettization process. This will
  -- improve image quality, but result in a larger
  -- file.
  , epFormatName :: Maybe String
  -- ^ FFmpeg muxer format name. If 'Nothing', tries to infer
  -- from the output file name. If 'Just', the string value
  -- should be the one available in @ffmpeg -formats@.
  }

-- | Minimal parameters describing the desired audio/video output.
data AVEncodingParams = AVEncodingParams
  { avepWidth  :: CInt
  , avepHeight :: CInt
  , avepFps    :: Int
  , avepCodec  :: Maybe AVCodecID
  -- ^ If 'Nothing', then the codec is inferred from
  -- the output file name. If 'Just', then this codec
  -- is manually chosen.
  , avepPixelFormat :: Maybe AVPixelFormat
  -- ^ If 'Nothing', automatically chose a pixel format
  -- based on the output codec. If 'Just', force the
  -- selected pixel format.
  , avepChannelLayout :: AVChannelLayout
  -- ^ Channel layout for the audio stream
  , avepSampleRate :: CInt
  -- ^ Sample rate for the audio stream
  , avepSampleFormat :: AVSampleFormat
  -- ^ Sample format for the audio stream
  , avepPreset :: String
  -- ^ Encoder-specific hints. For h264, the default
  -- preset is @\"medium\"@ (other options are
  -- @\"fast\"@, @\"slow\"@, etc.). For the GIF codec,
  -- setting this to @\"dither\"@ will enable dithering
  -- during the palettization process. This will
  -- improve image quality, but result in a larger
  -- file.
  , avepFormatName :: Maybe String
  -- ^ FFmpeg muxer format name. If 'Nothing', tries to infer
  -- from the output file name. If 'Just', the string value
  -- should be the one available in @ffmpeg -formats@.
  }

-- | Minimal parameters describing the desired audio/video output.
data AEncodingParams = AEncodingParams
  { aepChannelLayout :: AVChannelLayout
  -- ^ Channel layout for the audio stream
  , aepSampleRate :: CInt
  -- ^ Sample rate for the audio stream
  , aepSampleFormat :: AVSampleFormat
  -- ^ Sample format for the audio stream
  , aepPreset :: String
  -- ^ Encoder-specific hints. For h264, the default
  -- preset is @\"medium\"@ (other options are
  -- @\"fast\"@, @\"slow\"@, etc.). For the GIF codec,
  -- setting this to @\"dither\"@ will enable dithering
  -- during the palettization process. This will
  -- improve image quality, but result in a larger
  -- file.
  , aepFormatName :: Maybe String
  -- ^ FFmpeg muxer format name. If 'Nothing', tries to infer
  -- from the output file name. If 'Just', the string value
  -- should be the one available in @ffmpeg -formats@.
  }

data VideoParams = VideoParams
  { vpWidth  :: CInt
  , vpHeight :: CInt
  , vpFps    :: Int
  , vpCodec  :: Maybe AVCodecID
  , vpPixelFormat :: Maybe AVPixelFormat
  , vpPreset :: String
  }

class HasVideoParams a where
  extractVideoParams :: a -> VideoParams

instance HasVideoParams EncodingParams where
  extractVideoParams ep = VideoParams
    { vpWidth  = epWidth ep
    , vpHeight = epHeight ep
    , vpFps    = epFps ep
    , vpCodec  = epCodec ep
    , vpPixelFormat = epPixelFormat ep
    , vpPreset = epPreset ep
    }

instance HasVideoParams AVEncodingParams where
  extractVideoParams ep = VideoParams
    { vpWidth  = avepWidth ep
    , vpHeight = avepHeight ep
    , vpFps    = avepFps ep
    , vpCodec  = avepCodec ep
    , vpPixelFormat = avepPixelFormat ep
    , vpPreset = avepPreset ep
    }

class HasAudioParams a where
  extractAudioParams :: a -> AudioParams

instance HasAudioParams AEncodingParams where
  extractAudioParams ep = AudioParams
    { apChannelLayout = aepChannelLayout ep
    , apSampleRate = aepSampleRate ep
    , apSampleFormat = aepSampleFormat ep
    }

instance HasAudioParams AVEncodingParams where
  extractAudioParams ep = AudioParams
    { apChannelLayout = avepChannelLayout ep
    , apSampleRate = avepSampleRate ep
    , apSampleFormat = avepSampleFormat ep
    }

-- | Use default parameters for a video of the given width and
-- height, forcing the choice of the h264 encoder.
defaultH264 :: CInt -> CInt -> EncodingParams
defaultH264 w h =
  EncodingParams
    { epWidth = w
    , epHeight = h
    , epFps = 30
    , epCodec = (Just avCodecIdH264)
    , epPixelFormat = Nothing
    , epPreset = "medium"
    , epFormatName = Nothing
    }

-- | Use default parameters for a video of the given width and
-- height. The output format is determined by the output file name.
defaultParams :: CInt -> CInt -> EncodingParams
defaultParams w h =
  EncodingParams
    { epWidth = w
    , epHeight = h
    , epFps = 30
    , epCodec = Nothing
    , epPixelFormat = Nothing
    , epPreset = ""
    , epFormatName = Nothing
    }

-- | Determine if the bitwise intersection of two values is non-zero.
checkFlag :: Bits a => a -> a -> Bool
checkFlag flg = \x -> (flg .&. x) /= allZeroBits
  where allZeroBits = clearBit (bit 0) 0

-- | Find and initialize the requested encoder, and add a video stream
-- to the output container.
initStream :: EncodingParams -> AVFormatContext -> IO (AVStream, AVCodecContext)
initStream ep oc = initVideoStream (extractVideoParams ep) oc

initVideoStream :: VideoParams -> AVFormatContext -> IO (AVStream, AVCodecContext)
initVideoStream vp _
  | (vpWidth vp `rem` 2, vpHeight vp `rem` 2) /= (0,0) =
    error "Video dimensions must be multiples of two"
initVideoStream vp oc = do
  -- Use the codec suggested by the output format, or override with
  -- the user's choice.
  codec <- maybe (getOutputFormat oc >>= getVideoCodecID) return (vpCodec vp)
  cod <- avcodec_find_encoder codec
  when (getPtr cod == nullPtr)
       (error "Couldn't find encoder")

  st <- avformat_new_stream oc cod
  getNumStreams oc >>= setId st . subtract 1
  let framePeriod = AVRational 1 (fromIntegral $ vpFps vp)
      frameRate = AVRational (fromIntegral $ vpFps vp) 1
  setTimeBase st framePeriod
  ctx <- avcodec_alloc_context3 cod
  when (getPtr ctx == nullPtr) (error "Failed to allocate codec context")
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

  codecParams <- getCodecParams st
  runWithError "Could not copy params" (avcodec_parameters_from_context codecParams ctx)

  return (st, ctx)

initAudioStream :: AudioParams
                -> AVFormatContext
                -> IO (AVStream, AVCodec, AVCodecContext)
initAudioStream params oc = do
  codecId <- getAudioCodecID =<< getOutputFormat oc
  cod <- avcodec_find_encoder codecId
  when (getPtr cod == nullPtr) (avError "Could not find audio codec")

  st <- avformat_new_stream oc cod
  getNumStreams oc >>= setId st . subtract 1
  setTimeBase st (AVRational 1 (apSampleRate params))

  ctx <- avcodec_alloc_context3 cod

  supportedSampleRates <- listSupportedSampleFormats cod
  let found = not (null supportedSampleRates)
  when (not found) $ avError "Could not find supported sample rate"
  -- TODO: check that these are valid
  setSampleFormat ctx (apSampleFormat params)
  setSampleRate ctx (apSampleRate params)

  setChannelLayout ctx (apChannelLayout params)

  runWithError "Could not open audio codec" (open_codec ctx cod nullPtr)

  codecParams <- getCodecParams st
  runWithError "Could not copy params" (avcodec_parameters_from_context codecParams ctx)

#if LIBAVFORMAT_VERSION_MAJOR < 57
  getCodecFlags ctx >>= setCodecFlags ctx . (.|. codecFlagGlobalHeader)
#else
  getCodecFlags ctx >>= setCodecFlags ctx . (.|. avCodecFlagGlobalHeader)
#endif

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
  do
    --r <- avcodec_encode_video2 ctx pkt frame' gotOutput
    r <- avcodec_send_frame ctx frame'
    if (r == 0 || r == c_AVERROR_EAGAIN) then do 
      e <- avcodec_receive_packet ctx pkt
      pure (e /= c_AVERROR_EAGAIN)
    else if r == c_AVERROR_EOF then
      pure False
    else
      getError "Error encoding frame" r
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

{-# DEPRECATED frameWriter "Please use videoWriter instead." #-}
frameWriter :: EncodingParams -> FilePath
            -> IO (Maybe (AVPixelFormat, V2 CInt, Vector CUChar) -> IO ())
frameWriter ep fname = do
  let sp = JustVideo (extractVideoParams ep)
  writerContext <- avWriter (epFormatName ep) sp fname
  return (avwVideoWriter writerContext)

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
videoWriter :: EncodingParams -> FilePath
            -> IO (Maybe (AVPixelFormat, V2 CInt, Vector CUChar) -> IO ())
videoWriter ep fname = do
  let sp = JustVideo (extractVideoParams ep)
  writerContext <- avWriter (epFormatName ep) sp fname
  return (avwVideoWriter writerContext)

data StreamParams =
    JustVideo VideoParams
  | JustAudio AudioParams
  | AudioVideo AudioParams VideoParams

withVideoParams :: StreamParams -> a -> (VideoParams -> a) -> a
withVideoParams sp def f =
  case sp of
    JustVideo vp -> f vp
    AudioVideo _ vp -> f vp
    _ -> def

withAudioParams :: StreamParams -> a -> (AudioParams -> a) -> a
withAudioParams sp def f =
  case sp of
    JustAudio ap -> f ap
    AudioVideo ap _ -> f ap
    _ -> def

-- | Open a target for writing an audio stream.
audioWriter :: AEncodingParams
            -> FilePath
            -> IO (Maybe AVCodecContext, Maybe AVFrame -> IO ())
audioWriter ep fname = do
  let sp = JustAudio (extractAudioParams ep)
  writerContext <- avWriter (aepFormatName ep) sp fname
  return (avwAudioCodecContext writerContext, avwAudioWriter writerContext)

data AVWriterContext = AVWriterContext
  { avwVideoCodecContext :: Maybe AVCodecContext
  , avwAudioCodecContext :: Maybe AVCodecContext
  , avwVideoWriter :: Maybe (AVPixelFormat, V2 CInt, Vector CUChar) -> IO ()
  , avwAudioWriter :: Maybe AVFrame -> IO ()
  }

-- | Open a target for writing a video and audio file.
audioVideoWriter :: AVEncodingParams -> FilePath -> IO AVWriterContext
audioVideoWriter ep fname = do
  let sp = AudioVideo (extractAudioParams ep) (extractVideoParams ep)
  avWriter (avepFormatName ep) sp fname

-- | For internal use only. Use 'videoWriter', 'audioWriter', or 'audioVideoWriter' instead.
avWriter :: Maybe String
         -> StreamParams
         -> FilePath
         -> IO AVWriterContext
avWriter outputFormat sp fname = do
  oc <- allocOutputContext outputFormat fname
  outputFormat <- getOutputFormat oc
  audioCodecId <- getAudioCodecID outputFormat
  videoCodecId <- getVideoCodecID outputFormat

  -- Initializing the streams needs to be done before opening the file
  -- and checking the header because it can modify fields that are used
  -- for time scaling so we have this rather ugly code.
  mVideoStream <- withVideoParams sp (return Nothing) $ \vp ->
                    (Just <$> initVideoStream vp oc)
  mAudioStream <- withAudioParams sp (return Nothing) $ \ap ->
                    (Just <$> initAudioStream ap oc)
                    
  avio_open_check oc fname
  numStreams <- getNumStreams oc
  withCString fname (\str -> av_dump_format oc 0 str 1)
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
                      -> IO (Maybe AVFrame -> IO ())
      initializeAudio st codec ctx = do
        if audioCodecId /= avCodecIdNone
          then do
            pkt <- av_packet_alloc
            init_packet pkt

            frameNum <- newIORef (0::Int)
            timeBase <- getTimeBase ctx

            lastPts <- newIORef 0

            let read_pkts = do
                  ret <- avcodec_receive_packet ctx pkt
                  -- TODO: Distinguish between temp and permanent errors with EAGAIN
                  if ret /= 0
                    then return ()
                    else do
                      timeBase2 <- getTimeBase st
                      packet_rescale_ts pkt timeBase timeBase2
                      setStreamIndex pkt =<< getStreamIndex st
                      -- TODO: Not sure this pts will be exactly accurate.
                      -- Also, we need to set duration too because it doesn't seem to be set.
                      setPts pkt =<< readIORef lastPts
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
                  setPts frame (av_rescale_q samplesCount (AVRational 1 sampleRate) timeBase)
                  newPts <- getPts frame
                  modifyIORef lastPts (const newPts)
                  modifyIORef frameNum (+ fromIntegral numSamples)

                  runWithError "Error encoding audio"
                      (avcodec_send_frame ctx frame)
                  read_pkts
            return writeAudioFrame
          else
            return $ \_ -> return ()

  videoWriter <- case mVideoStream of
                   Just (vs, ctx) ->
                     withVideoParams sp (return (\_ -> return ()))
                       (initializeVideo vs ctx)
                   Nothing -> return (\_ -> return ())
  audioWriter <- case mAudioStream of
                   Just (as, codec, ctx) ->
                     withAudioParams sp (return $ \_ -> return ())
                       (const (initializeAudio as codec ctx))
                   Nothing -> return $ \_ -> return ()

  return $ AVWriterContext
    { avwVideoCodecContext = snd <$> mVideoStream
    , avwAudioCodecContext = (\(_, _, ctx) -> ctx) <$> mAudioStream
    , avwVideoWriter = videoWriter
    , avwAudioWriter = audioWriter
    }

-- | Open a target file for writing a video stream. The function
-- returned may be used to write RGB images of the resolution given by
-- the provided 'EncodingParams' (i.e. the same resolution as the
-- output video). If this function is applied to 'Nothing', then the
-- output stream is closed. Note that 'Nothing' /must/ be provided to
-- properly terminate video encoding. Throws an error if you do not
-- provide a 'EncodingParams' without 'VideoParams'
frameWriterRgb :: EncodingParams -> FilePath
               -> IO (Maybe (Vector CUChar) -> IO ())
frameWriterRgb ep f = do
  let aux pixels = (avPixFmtRgb24, V2 (epWidth ep) (epHeight ep), pixels)
  videoWriter <- frameWriter ep f
  return $ \pix -> videoWriter (aux <$> pix)
