{-# LANGUAGE ForeignFunctionInterface #-}
-- | Video encoding API. Includes FFI declarations for the underlying
-- FFmpeg functions, wrappers for these functions that wrap error
-- condition checking, and high level Haskellized interfaces.
module Codec.FFmpeg.Encode where
import Codec.FFmpeg.Common
import Codec.FFmpeg.Enums
import Codec.FFmpeg.Scaler
import Codec.FFmpeg.Types
import Codec.Picture
import Control.Applicative
import Control.Monad (when)
import Control.Monad.Error.Class
import Data.Bits
import Data.Maybe (fromMaybe)
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
  guess_format :: CString -> CString -> CString -> IO AVOutputFormat

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

-- * FFmpeg Encoding Interface

-- | Minimal parameters describing the desired video output.
data EncodingParams = EncodingParams { epWidth  :: CInt
                                     , epHeight :: CInt
                                     , epFps    :: Int
                                     , epCodec  :: AVCodecID
                                     , epPreset :: String }

-- | Use default parameters for a video of the given width and
-- height. This produces an h264 encoded stream.
defaultParams :: CInt -> CInt -> EncodingParams
defaultParams w h = EncodingParams w h 30 avCodecIdH264 "medium"

-- | Find and initialize the requested encoder, and add a video stream
-- to the output container.
initStream :: EncodingParams -> AVFormatContext -> IO (AVStream, AVCodecContext)
initStream ep _
  | (epWidth ep `rem` 2, epHeight ep `rem` 2) /= (0,0) =
    throwError $ strMsg "Video dimensions must be multiples of two"
initStream ep oc = do
  cod <- avcodec_find_encoder (epCodec ep)
  when (getPtr cod == nullPtr)
       (throwError $ strMsg "Couldn't find H264 encoder")

  st <- avformat_new_stream oc cod
  getNumStreams oc >>= setId st . subtract 1

  ctx <- getCodecContext st
  setWidth ctx (epWidth ep)
  setHeight ctx (epHeight ep)
  let framePeriod = AVRational 1 (fromIntegral $ epFps ep)
  setTimeBase ctx framePeriod
  setPixelFormat ctx avPixFmtYuv420p
  
  -- Some formats want stream headers to be separate
  outputFlags <- getOutputFormat oc >>= getFormatFlags
  when (outputFlags .&. avfmtGlobalheader /= clearBit (bit 0) 0) $
    getCodecFlags ctx >>= setCodecFlags ctx . (.|. codecFlagGlobalHeader)

  -- _ <- withCString "vprofile" $ \kStr ->
  --        withCString (preset ep) $ \vStr ->
  --          av_opt_set ((#ptr AVCodecContext, priv_data) (getPtr ctx))
  --                     kStr vStr 0
  -- _ <- withCString "preset" $ \kStr ->
  --        withCString "medium" $ \vStr ->
  --          av_opt_set ((#ptr AVCodecContext, priv_data) (getPtr ctx))
  --                     kStr vStr 0

  rOpen <- open_codec ctx cod nullPtr
  when (rOpen < 0) (throwError $ strMsg "Couldn't open codec")
  return (st, ctx)

-- | Initialize a temporary YUV frame of the same resolution as the
-- output video stream. We well convert RGB frames using this frame as
-- a destination before encoding the video frame.
initTempYuv :: EncodingParams -> IO AVFrame
initTempYuv ep = do
  yuv <- frame_alloc_check
  setPixelFormat yuv avPixFmtYuv420p
  setWidth yuv (epWidth ep)
  setHeight yuv (epHeight ep)
  setPts yuv 0
  frame_get_buffer_check yuv 32
  return yuv

-- | Allocate an output context inferring the codec from the given
-- file name.
allocOutputContext :: FilePath -> IO AVFormatContext
allocOutputContext fname = do
  oc <- alloca $ \ocTmp ->
          withCString fname $ \fname' -> do
            r <- avformat_alloc_output_context ocTmp (AVOutputFormat nullPtr)
                                               nullPtr fname'
            when (r < 0)
                 (throwError $ strMsg "Couldn't allocate output format context")
            peek ocTmp
  when (getPtr oc == nullPtr)
       (throwError $ strMsg "Couldn't allocate output AVFormatContext")
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

  yuv <- initTempYuv ep

  -- Initialize the scaler that we use to convert RGB -> YUV  
  sws <- swsInit (ImageInfo (epWidth ep) (epHeight ep) avPixFmtRgb24)
                 (ImageInfo (epWidth ep) (epHeight ep) avPixFmtYuv420p)
                 swsBilinear

  pkt <- AVPacket <$> mallocBytes packetSize

  stIndex <- getStreamIndex st
  avio_open_check oc fname
  write_header_check oc

  tb <- getTimeBase st
  codecTB <- getCodecContext st >>= getTimeBase
  let frameTime = av_rescale_q 1 codecTB tb
      mkImage :: Vector CUChar -> Image PixelRGB8
      mkImage = let [w,h] = map fromIntegral [epWidth ep, epHeight ep]
                in Image w h . V.unsafeCast
      resetPacket = do init_packet pkt
                       setData pkt nullPtr
                       setSize pkt 0
      writePacket = do setStreamIndex pkt stIndex
                       write_frame_check oc pkt
      go Nothing = do
        resetPacket
        goOn <- encode_video_check ctx pkt Nothing
        if goOn
        then writePacket >> go Nothing
        else do write_trailer_check oc
                _ <- codec_close ctx
                with yuv av_frame_free
                avio_close_check oc
                avformat_free_context oc

      go (Just pixels) = do
        resetPacket
        _ <- swsScale sws (mkImage pixels) yuv

        getPts yuv >>= setPts yuv . (+ frameTime)
        encode_video_check ctx pkt (Just yuv) >>= flip when writePacket
  return go
