{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances,
             GeneralizedNewtypeDeriving #-}
module Codec.FFmpeg.Types where
import Codec.FFmpeg.Enums
import Control.Monad (zipWithM_,when)
import Data.Maybe (fromMaybe)
import Foreign.C.String (CString)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc (malloc)

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/audio_fifo.h>
#include <libavutil/avutil.h>
#include <libswscale/swscale.h>
#include <libswresample/swresample.h>
#include "hscMacros.h"
#include "nameCompat.h"

class HasPtr a where
  getPtr :: a -> Ptr ()

instance HasPtr (Ptr ()) where getPtr = id

newtype AVFormatContext = AVFormatContext (Ptr ()) deriving (Storable, HasPtr)
#mkField NumStreams, CInt
#mkField Streams, (Ptr AVStream)
#mkField OutputFormat, AVOutputFormat
#mkField IOContext, AVIOContext
#mkField InputFormat, AVInputFormat

#hasField AVFormatContext, NumStreams, nb_streams
#hasField AVFormatContext, Streams, streams
#hasField AVFormatContext, OutputFormat, oformat
#hasField AVFormatContext, InputFormat, iformat
#hasField AVFormatContext, IOContext, pb
#hasField AVFormatContext, VideoCodecID, video_codec_id

setFilename :: AVFormatContext -> String -> IO ()
setFilename ctx fn =
    do let ptr  = getPtr ctx
           dst   = (#ptr AVFormatContext, filename) ptr
           bytes = map (fromIntegral . fromEnum) fn
       zipWithM_ (pokeElemOff dst) bytes [(0 :: CInt) ..]


foreign import ccall "av_input_video_device_next"
  av_input_video_device_next :: AVInputFormat -> IO AVInputFormat

setCamera :: AVFormatContext -> IO ()
setCamera ctx = do
    ipt <- getCameraAVInputFormat (AVInputFormat nullPtr)
    setInputFormat ctx ipt
  where
    -- Currently straight-line, but we can filter each 'nxt' based on
    -- predicates, such as device ('avfoundtion', 'v4l2' etc) in the
    -- future, if needed.
    getCameraAVInputFormat :: AVInputFormat -> IO AVInputFormat
    getCameraAVInputFormat p = do
        nxt <- av_input_video_device_next p
        when (nullPtr == getPtr nxt) (error "No video input device found.")
        return nxt

foreign import ccall "avformat_alloc_context"
    avformat_alloc_context :: IO (Ptr ())

mallocAVFormatContext :: IO AVFormatContext
mallocAVFormatContext = AVFormatContext <$> avformat_alloc_context

newtype AVCodecContext = AVCodecContext (Ptr ()) deriving (Storable, HasPtr)

foreign import ccall "avcodec_alloc_context3"
  avcodec_alloc_context3 :: AVCodec -> IO AVCodecContext

#mkField BitRate, CInt

#mkField SampleFormat, AVSampleFormat
#mkField Width, CInt
#mkField Height, CInt
#mkField TimeBase, AVRational
#mkField GopSize, CInt
#mkField PixelFormat, AVPixelFormat
#mkField CodecFlags, CodecFlag
#mkField CodecID, AVCodecID
#mkField PrivData, (Ptr ())
#mkField TicksPerFrame, CInt
#mkField RawAspectRatio, AVRational
#mkField SampleRate, CInt
#mkField ChannelLayout, CULong
#mkField Channels, CInt
#mkField FrameSize, CInt
#mkField FrameRate, AVRational

#hasField AVCodecContext, BitRate, bit_rate
#hasField AVCodecContext, Width, width
#hasField AVCodecContext, Height, height
#hasField AVCodecContext, TimeBase, time_base
#hasField AVCodecContext, GopSize, gop_size
#hasField AVCodecContext, PixelFormat, pix_fmt
#hasField AVCodecContext, CodecFlags, flags
#hasField AVCodecContext, CodecID, codec_id
#hasField AVCodecContext, PrivData, priv_data
#hasField AVCodecContext, TicksPerFrame, ticks_per_frame
#hasField AVCodecContext, RawAspectRatio, sample_aspect_ratio
#hasField AVCodecContext, SampleRate, sample_rate
#hasField AVCodecContext, ChannelLayout, channel_layout
#hasField AVCodecContext, Channels, channels
#hasField AVCodecContext, SampleFormat, sample_fmt
#hasField AVCodecContext, FrameSize, frame_size
#hasField AVCodecContext, FrameRate, framerate

getFps :: (HasTimeBase a, HasTicksPerFrame a) => a -> IO CDouble
getFps x = do
  timeBase <- getTimeBase x
  ticksPerFrame <- getTicksPerFrame x
  pure (1.0 / av_q2d timeBase / fromIntegral ticksPerFrame)

getAspectRatio :: HasRawAspectRatio a => a -> IO (Maybe AVRational)
getAspectRatio = fmap nonZeroAVRational . getRawAspectRatio

-- | When unspecified, the most likely pixel shape is a square
guessAspectRatio :: HasRawAspectRatio a => a -> IO AVRational
guessAspectRatio = fmap (fromMaybe (AVRational 1 1)) . getAspectRatio

setAspectRatio :: HasRawAspectRatio a => a -> Maybe AVRational -> IO ()
setAspectRatio x Nothing      = setRawAspectRatio x (AVRational 0 1)
setAspectRatio x (Just ratio) = setRawAspectRatio x ratio

newtype AVCodecParameters = AVCodecParameters (Ptr ()) deriving (Storable, HasPtr)

foreign import ccall "avcodec_parameters_from_context"
  avcodec_parameters_from_context :: AVCodecParameters
                                  -> AVCodecContext
                                  -> IO CInt

newtype AVStream = AVStream (Ptr ()) deriving (Storable, HasPtr)
#mkField Id, CInt
#mkField CodecContext, AVCodecContext
#mkField StreamIndex, CInt
#mkField CodecParams, AVCodecParameters

#hasField AVStream, Id, id
#hasField AVStream, TimeBase, time_base
#hasField AVStream, CodecContext, codec
#hasField AVStream, StreamIndex, index
#hasField AVStream, CodecParams, codecpar

newtype AVCodec = AVCodec (Ptr ()) deriving (Storable, HasPtr)
#mkField LongName, CString
#mkField Name, CString
#mkField PixelFormats, (Ptr AVPixelFormat)
#mkField SampleFormats, (Ptr AVSampleFormat)
#mkField ChannelLayouts, (Ptr CULong)
#mkField SupportedSampleRates, (Ptr CInt)
#mkField Capabilities, CInt

#hasField AVCodec, LongName, long_name
#hasField AVCodec, Name, name
#hasField AVCodec, CodecID, id
#hasField AVCodec, PixelFormats, pix_fmts
#hasField AVCodec, SampleFormats, sample_fmts
#hasField AVCodec, ChannelLayouts, channel_layouts
#hasField AVCodec, SupportedSampleRates, supported_samplerates
#hasField AVCodec, Capabilities, capabilities

newtype AVDictionary = AVDictionary (Ptr ()) deriving (Storable, HasPtr)
newtype AVFrame = AVFrame (Ptr ()) deriving (Storable, HasPtr)
#mkField Pts, CLong
#mkField PktPts, CLong
#mkField LineSize, CInt
#mkField Data, (Ptr (Ptr ()))
#mkField ExtendedData, (Ptr (Ptr ()))
#mkField NumSamples, CInt
#mkField Format, CInt

#hasField AVFrame, PixelFormat, format
#hasField AVFrame, SampleFormat, format
#hasField AVFrame, Width, width
#hasField AVFrame, Height, height
#hasField AVFrame, LineSize, linesize
#hasField AVFrame, Pts, pts
#hasField AVFrame, PktPts, pkt_pts
#hasField AVFrame, Data, data
#hasField AVFrame, ExtendedData, extended_data
#hasField AVFrame, NumSamples, nb_samples
#hasField AVFrame, Format, format
#hasField AVFrame, Channels, channels
#hasField AVFrame, ChannelLayout, channel_layout
#hasField AVFrame, SampleRate, sample_rate

newtype AVPicture = AVPicture (Ptr ()) deriving (Storable, HasPtr)
#hasField AVPicture, Data, data

newtype SwsContext = SwsContext (Ptr ()) deriving (Storable, HasPtr)
newtype AVOutputFormat = AVOutputFormat (Ptr ()) deriving (Storable, HasPtr)
#mkField FormatFlags, FormatFlag
#mkField VideoCodecID, AVCodecID
#mkField AudioCodecID, AVCodecID
#hasField AVOutputFormat, FormatFlags, flags
#hasField AVOutputFormat, VideoCodecID, video_codec
#hasField AVOutputFormat, AudioCodecID, audio_codec

newtype AVInputFormat = AVInputFormat (Ptr ()) deriving (Storable, HasPtr)
newtype AVClass = AVClass (Ptr ()) deriving (Storable, HasPtr)
#mkField AVClass, AVClass
#hasField AVInputFormat, AVClass, priv_class

#if LIBAVUTIL_VERSION_MAJOR > 52
getAVCategory :: AVInputFormat -> IO Category
getAVCategory aif =
  do c <- getAVClass aif
     if nullPtr == getPtr c
        then return (Category (-1))
        else Category <$> peek ((#ptr AVClass, category) $ castPtr $ getPtr c)

newtype Category = Category CInt deriving (Eq,Ord,Show,Read,Enum)
#enum Category, Category, AV_CLASS_CATEGORY_NA, AV_CLASS_CATEGORY_INPUT,\
        AV_CLASS_CATEGORY_OUTPUT, AV_CLASS_CATEGORY_MUXER, AV_CLASS_CATEGORY_DEMUXER,\
        AV_CLASS_CATEGORY_ENCODER, AV_CLASS_CATEGORY_DECODER, AV_CLASS_CATEGORY_FILTER,\
        AV_CLASS_CATEGORY_BITSTREAM_FILTER, AV_CLASS_CATEGORY_SWSCALER, AV_CLASS_CATEGORY_SWRESAMPLER,\
        AV_CLASS_CATEGORY_DEVICE_VIDEO_OUTPUT, AV_CLASS_CATEGORY_DEVICE_VIDEO_INPUT, AV_CLASS_CATEGORY_DEVICE_AUDIO_OUTPUT,\
        AV_CLASS_CATEGORY_DEVICE_AUDIO_INPUT, AV_CLASS_CATEGORY_DEVICE_OUTPUT, AV_CLASS_CATEGORY_DEVICE_INPUT,\
        AV_CLASS_CATEGORY_NB
#endif

newtype AVIOContext = AVIOContext (Ptr ()) deriving (Storable, HasPtr)

newtype AVPacket = AVPacket (Ptr ()) deriving (Storable, HasPtr)
#mkField PktData, (Ptr ())
#mkField Size, CInt
#mkField PacketFlags, PacketFlag
#mkField Dts, CLong
#mkField Duration, CULong

#hasField AVPacket, PktData, data
#hasField AVPacket, Size, size
#hasField AVPacket, PacketFlags, flags
#hasField AVPacket, StreamIndex, stream_index
#hasField AVPacket, Pts, pts
#hasField AVPacket, Dts, dts
#hasField AVPacket, Duration, duration

-- | @sizeof@ the 'AVPacket' structure in bytes.
packetSize :: Int
packetSize = #size AVPacket

pictureSize :: Int
pictureSize = #size AVPicture

newtype SwrContext = SwrContext (Ptr ()) deriving (Storable, HasPtr)

newtype AVAudioFifo = AVAudioFifo (Ptr ()) deriving (Storable, HasPtr)

foreign import ccall "av_samples_alloc_array_and_samples"
  av_samples_alloc_array_and_samples :: Ptr (Ptr (Ptr CUChar))
                                     -> Ptr CInt
                                     -> CInt
                                     -> CInt
                                     -> AVSampleFormat
                                     -> CInt
                                     -> IO CInt

foreign import ccall "av_audio_fifo_free"
  av_audio_fifo_free :: AVAudioFifo -> IO ()

foreign import ccall "av_audio_fifo_alloc"
  av_audio_fifo_alloc :: AVSampleFormat -> CInt -> CInt -> IO AVAudioFifo

foreign import ccall "av_audio_fifo_realloc"
  av_audio_fifo_realloc :: AVAudioFifo -> CInt -> IO CInt

foreign import ccall "av_audio_fifo_write"
  av_audio_fifo_write :: AVAudioFifo -> Ptr (Ptr ()) -> CInt -> IO CInt

foreign import ccall "av_audio_fifo_peek"
  av_audio_fifo_peek :: AVAudioFifo -> Ptr (Ptr ()) -> CInt -> IO CInt

foreign import ccall "av_audio_fifo_peek_at"
  av_audio_fifo_peek_at :: AVAudioFifo -> Ptr (Ptr ()) -> CInt -> CInt -> IO CInt

foreign import ccall "av_audio_fifo_read"
  av_audio_fifo_read :: AVAudioFifo -> Ptr (Ptr ()) -> CInt -> IO CInt

foreign import ccall "av_audio_fifo_drain"
  av_audio_fifo_drain :: AVAudioFifo -> CInt -> IO CInt

foreign import ccall "av_audio_fifo_reset"
  av_audio_fifo_reset :: AVAudioFifo -> IO ()

foreign import ccall "av_audio_fifo_size"
  av_audio_fifo_size :: AVAudioFifo -> IO CInt

foreign import ccall "av_audio_fifo_space"
  av_audio_fifo_space :: AVAudioFifo -> IO CInt

-- * Types with Haskell equivalents

data AVRational = AVRational { numerator   :: CInt
                             , denomenator :: CInt } deriving Show

-- | FFmpeg often uses 0 to mean "unknown"; use 'Nothing' instead.VRational
nonZeroAVRational :: AVRational -> Maybe AVRational
nonZeroAVRational (AVRational 0 _) = Nothing
nonZeroAVRational ratio            = Just ratio

instance Storable AVRational where
  sizeOf _ = #size AVRational
  alignment _ = #size AVRational
  peek ptr = AVRational <$> (#peek AVRational, num) ptr
                        <*> (#peek AVRational, den) ptr
  poke ptr (AVRational n d) = do (#poke AVRational, num) ptr n
                                 (#poke AVRational, den) ptr d

foreign import ccall "av_rescale_rnd"
  av_rescale_rnd :: CLong -> CLong -> CLong -> AVRoundMode -> CLong

-- | Convert an 'AVRational' to a 'Double'
av_q2d :: AVRational -> CDouble
av_q2d r = fromIntegral (numerator r) / fromIntegral (denomenator r)

-- | Rescale an integer from one time base to another.
av_rescale_q :: CLong -> AVRational -> AVRational -> CLong
av_rescale_q a bq cq = av_rescale_rnd a b c avRoundNearInf
  where b = fromIntegral (numerator bq) * fromIntegral (denomenator cq)
        c = fromIntegral (numerator cq) * fromIntegral (denomenator bq)

foreign import ccall "av_packet_rescale_ts"
  av_packet_rescale_ts :: AVPacket -> Ptr AVRational -> Ptr AVRational -> IO ()

foreign import ccall "av_packet_unref"
  av_packet_unref :: AVPacket -> IO ()

packet_rescale_ts :: AVPacket -> AVRational -> AVRational -> IO ()
packet_rescale_ts packet rat1 rat2 = do
  ptr1 <- malloc
  ptr2 <- malloc
  poke ptr1 rat1
  poke ptr2 rat2
  av_packet_rescale_ts packet ptr1 ptr2


#if LIBAVFORMAT_VERSION_MAJOR < 57
data AVFrac = AVFrac { fracVal :: CLong
                     , fracNum :: CLong
                     , fracDen :: CLong } deriving Show

instance Storable AVFrac where
  sizeOf _ = #size AVFrac
  alignment _ = #size AVFrac
  peek ptr = AVFrac <$> (#peek AVFrac, val) ptr
                    <*> (#peek AVFrac, num) ptr
                    <*> (#peek AVFrac, den) ptr
  poke ptr (AVFrac v n d) = do (#poke AVFrac, val) ptr v
                               (#poke AVFrac, num) ptr n
                               (#poke AVFrac, den) ptr d
#endif

-- | The input source can be a file or a camera.  When using 'Camera',
-- frequently in the form @Camera "0:0" defaultCameraConfig@, the first input video device
-- enumerated by libavdevice is selected.
data InputSource = File FilePath | Camera String CameraConfig
            deriving (Eq, Ord, Show, Read)

data CameraConfig =
  CameraConfig { framerate  :: Maybe Int
               , resolution :: Maybe (Int,Int)
               , format :: Maybe String
               -- ^ Short name for a video format (e.g. @"v4l2"@)
               }
          deriving (Eq,Ord,Show,Read)

defaultCameraConfig :: CameraConfig
defaultCameraConfig = CameraConfig (Just 30) Nothing Nothing
