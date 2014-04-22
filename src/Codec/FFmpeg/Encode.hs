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
import Codec.FFmpeg.Internal.V3
import Codec.FFmpeg.Scaler
import Codec.FFmpeg.Types
import Codec.Picture
import Control.Applicative
import Control.Monad (when, void)
import Control.Monad.Error.Class
import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils

import Foreign.Ptr
import Foreign.Storable

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
  avformat_alloc_output_context :: Ptr AVFormatContext -> AVOutputFormat
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

-- * FFmpeg Encoding Interface

-- | Minimal parameters describing the desired video output.
data EncodingParams = EncodingParams { epWidth  :: CInt
                                     , epHeight :: CInt
                                     , epFps    :: Int
                                     , epCodec  :: Maybe AVCodecID
                                     -- ^ If 'Nothing', then the codec
                                     -- is inferred from the output
                                     -- file name. If 'Just', then
                                     -- this codec is manually chosen.
                                     , epPreset :: String }

-- | Use default parameters for a video of the given width and
-- height, forcing the choice of the h264 encoder.
defaultH264 :: CInt -> CInt -> EncodingParams
defaultH264 w h = EncodingParams w h 30 (Just avCodecIdH264) "medium"

-- | Use default parameters for a video of the given width and
-- height. The output format is determined by the output file name.
defaultParams :: CInt -> CInt -> EncodingParams
defaultParams w h = EncodingParams w h 30 Nothing ""

-- | Determine if the bitwise intersection of two values is non-zero.
checkFlag :: Bits a => a -> a -> Bool
checkFlag flg = \x -> (flg .&. x) /= allZeroBits
  where allZeroBits = clearBit (bit 0) 0

-- | Find and initialize the requested encoder, and add a video stream
-- to the output container.
initStream :: EncodingParams -> AVFormatContext -> IO (AVStream, AVCodecContext)
initStream ep _
  | (epWidth ep `rem` 2, epHeight ep `rem` 2) /= (0,0) =
    throwError $ strMsg "Video dimensions must be multiples of two"
initStream ep oc = do
  -- Use the codec suggested by the output format, or override with
  -- the user's choice.
  codec <- maybe (getOutputFormat oc >>= getVideoCodecID) return (epCodec ep)
  cod <- avcodec_find_encoder codec
  when (getPtr cod == nullPtr)
       (errMsg "Couldn't find encoder")

  st <- avformat_new_stream oc cod
  getNumStreams oc >>= setId st . subtract 1

  ctx <- getCodecContext st
  setWidth ctx (epWidth ep)
  setHeight ctx (epHeight ep)
  let framePeriod = AVRational 1 (fromIntegral $ epFps ep)
  setTimeBase ctx framePeriod
  setPixelFormat ctx $ case () of
                         _ | codec == avCodecIdRawvideo -> avPixFmtRgb24
                           | codec == avCodecIdGif -> avPixFmtRgb8
                           | otherwise -> avPixFmtYuv420p

  -- Some formats want stream headers to be separate
  needsHeader <- checkFlag avfmtGlobalheader <$> 
                 (getOutputFormat oc >>= getFormatFlags)
  when needsHeader $ 
    getCodecFlags ctx >>= setCodecFlags ctx . (.|. codecFlagGlobalHeader)

  -- _ <- withCString "vprofile" $ \kStr ->
  --        withCString (preset ep) $ \vStr ->
  --          av_opt_set ((#ptr AVCodecContext, priv_data) (getPtr ctx))
  --                     kStr vStr 0
  when (not . null $ epPreset ep) . void $
    withCString "preset" $ \kStr ->
      withCString (epPreset ep) $ \vStr ->
        getPrivData ctx >>= \pd -> av_opt_set pd kStr vStr 0

  rOpen <- open_codec ctx cod nullPtr
  when (rOpen < 0) (throwError $ strMsg "Couldn't open codec")

  return (st, ctx)

-- | Initialize a temporary YUV frame of the same resolution as the
-- output video stream. We well convert RGB frames using this frame as
-- a destination before encoding the video frame.
initTempFrame :: EncodingParams -> AVPixelFormat -> IO AVFrame
initTempFrame ep fmt = do
  yuv <- frame_alloc_check
  setPixelFormat yuv fmt
  setWidth yuv (epWidth ep)
  setHeight yuv (epHeight ep)
  setPts yuv 0
  frame_get_buffer_check yuv 32
  return yuv

-- | Allocate an output context inferring the codec from the given
-- file name.
allocOutputContext :: FilePath -> IO AVFormatContext
allocOutputContext fname = do
  oc <- alloca $ \ocTmp -> do
          r <- withCString fname $ \fname' -> 
                 avformat_alloc_output_context
                   ocTmp (AVOutputFormat nullPtr)
                   nullPtr fname'
          when (r < 0)
               (errMsg "Couldn't allocate output format context")
          peek ocTmp
  when (getPtr oc == nullPtr)
       (errMsg "Couldn't allocate output AVFormatContext")
  return oc

-- | Open the given file for writing.
avio_open_check :: AVFormatContext -> String -> IO ()
avio_open_check oc fname =
  do r <- withCString fname $ \cstr ->
            avio_open (hasIOContext oc) cstr avioFlagWrite
     when (r < 0) (errMsg "Error opening IO for writing")

-- | Close an open IO context.
avio_close_check :: AVFormatContext -> IO ()
avio_close_check oc = do r <- getIOContext oc >>= avio_close
                         when (r /= 0) (errMsg "Error closing IO")

-- | Returns 'True' if the 'AVPacket' was updated with new output
-- data; 'False' otherwise.
encode_video_check :: AVCodecContext -> AVPacket -> Maybe AVFrame -> IO Bool
encode_video_check ctx pkt frame =
  alloca $ \gotOutput -> do
    r <- avcodec_encode_video2 ctx pkt frame' gotOutput
    when (r < 0) (errMsg "Error encoding frame")
    (> 0) <$> peek gotOutput
  where frame' = fromMaybe (AVFrame nullPtr) frame

-- | Allocate the stream private data and write the stream header to
-- an output media file.
write_header_check :: AVFormatContext -> IO ()
write_header_check oc = do r <- avformat_write_header oc nullPtr
                           when (r < 0) (errMsg "Error writing header")

-- | Write a packet to an output media file.
write_frame_check :: AVFormatContext -> AVPacket -> IO ()
write_frame_check oc pkt = do r <- av_write_frame oc pkt
                              when (r < 0) (errMsg "Error writing frame")

-- | Write the stream trailer to an output media file and free the
-- private data. May only be called after a successful call to
-- 'write_header_check'.
write_trailer_check :: AVFormatContext -> IO ()
write_trailer_check oc = do r <- av_write_trailer oc
                            when (r /= 0) (errMsg "Error writing trailer")

-- | Quantize RGB24 pixels to the systematic RGB8 color palette. This
-- is slow, but lets us prepare an RGB24 pixel format for output as an
-- RGB8 GIF.
palettizeRGB8 :: V.Vector (V3 CUChar) -> V.Vector CUChar
palettizeRGB8 = V.map (searchPal . fmap fromIntegral)
  where pal :: V.Vector (V3 CInt)
        pal = V.generate 256 $ \i' -> 
                let i = fromIntegral i'
                in V3 ((i `shiftR` 5) * 36)
                      (((i `shiftR` 2) .&. 7) * 36)
                      ((i .&. 3) * 85)
        searchPal = fromIntegral . flip V.minIndexBy pal . comparing . qd

-- | Open a target file for writing a video stream. The function
-- returned may be used to write RGB images of the resolution given by
-- the provided 'EncodingParams'. The function will convert the
-- supplied RGB frame to YUV (specifically, @yuv420p@) before encoding
-- the image to the video stream. If this function is applied to
-- 'Nothing', then the output stream is closed. Note that 'Nothing'
-- /must/ be provided to properly terminate video encoding.
frameWriter :: EncodingParams -> FilePath -> IO (Maybe (Vector CUChar) -> IO ())
frameWriter ep fname = do
  oc <- allocOutputContext fname
  (st,ctx) <- initStream ep oc

  dstFmt <- getPixelFormat ctx
  dstFrame <- initTempFrame ep dstFmt

  -- Initialize the scaler that we use to convert RGB -> dstFmt
  -- Note that libswscaler does not support Pal8 as an output format.
  sws <- if dstFmt /= avPixFmtPal8 && dstFmt /= avPixFmtRgb8
         then Just <$> 
              swsInit (ImageInfo (epWidth ep) (epHeight ep) avPixFmtRgb24)
                      (ImageInfo (epWidth ep) (epHeight ep) dstFmt)
                      swsBilinear
         else return Nothing

  pkt <- AVPacket <$> mallocBytes packetSize

  stIndex <- getStreamIndex st
  avio_open_check oc fname
  write_header_check oc

  tb <- getTimeBase st
  codecTB <- getCodecContext st >>= getTimeBase
  isRaw <- checkFlag avfmtRawpicture <$> (getOutputFormat oc >>= getFormatFlags)

  let frameTime = av_rescale_q 1 codecTB tb
      mkImage :: Vector CUChar -> Image PixelRGB8
      mkImage = let [w,h] = map fromIntegral [epWidth ep, epHeight ep]
                in Image w h . V.unsafeCast
      resetPacket = do init_packet pkt
                       setData pkt nullPtr
                       setSize pkt 0
      writePacket = do setStreamIndex pkt stIndex
                       write_frame_check oc pkt
      copyDstData
        | dstFmt == avPixFmtRgb8 = copyDstDataAux . palettizeRGB8 . V.unsafeCast
        | otherwise = copyDstDataAux

      copyDstDataAux pixels =
        void . V.unsafeWith pixels $ \ptr ->
          av_image_fill_arrays (castPtr $ hasData dstFrame)
                               (hasLineSize dstFrame)
                               (castPtr ptr)
                               dstFmt
                               (epWidth ep)
                               (epHeight ep)
                               1
      scaleToDst sws' pixels = void $ swsScale sws' (mkImage pixels) dstFrame
      fillDst = maybe copyDstData scaleToDst sws
      addRaw Nothing = return ()
      addRaw (Just pixels) =
        do resetPacket
           getPacketFlags pkt >>= setPacketFlags pkt . (.|. avPktFlagKey)
           --setSize pkt (fromIntegral $ V.length pixels)
           setSize pkt (fromIntegral pictureSize)
           getPts dstFrame >>= setPts dstFrame . (+ frameTime)
           getPts dstFrame >>= setPts pkt
           getPts dstFrame >>= setDts pkt
           V.unsafeWith pixels $ \ptr -> do
             setData pkt (castPtr ptr)
             writePacket
      addEncoded Nothing = do resetPacket
                              encode_video_check ctx pkt Nothing >>=
                                flip when (writePacket >> addEncoded Nothing)
      addEncoded (Just pixels) =
        do resetPacket
           fillDst pixels
           getPts dstFrame >>= setPts dstFrame . (+ frameTime)
           encode_video_check ctx pkt (Just dstFrame) >>= flip when writePacket
      addFrame = if isRaw then addRaw else addEncoded
      go Nothing = do addFrame Nothing
                      write_trailer_check oc
                      _ <- codec_close ctx
                      with dstFrame av_frame_free
                      avio_close_check oc
                      avformat_free_context oc
      go (Just pixels) = addFrame (Just pixels)
  return go
