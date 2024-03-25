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
import Control.Monad.IO.Class (liftIO, MonadIO)

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
#mkField VideoCodec, AVCodec
#mkField AudioCodec, AVCodec

#hasField AVFormatContext, NumStreams, nb_streams
#hasField AVFormatContext, Streams, streams
#hasField AVFormatContext, OutputFormat, oformat
#hasField AVFormatContext, InputFormat, iformat
#hasField AVFormatContext, IOContext, pb
#hasField AVFormatContext, VideoCodecID, video_codec_id
#hasField AVFormatContext, VideoCodec, video_codec
#hasField AVFormatContext, AudioCodec, audio_codec

structMetadata :: (MonadIO m, HasPtr a) => a -> m (Maybe AVDictionary)
structMetadata ctx = do
  dict@(AVDictionary dictPtr) <- liftIO $ (#peek AVFormatContext, metadata) (getPtr ctx)
  pure $ if dictPtr == nullPtr then Nothing else (Just dict)

setUrl :: AVFormatContext -> String -> IO ()
setUrl ctx fn =
    do let ptr  = getPtr ctx
           dst   = (#ptr AVFormatContext, url) ptr
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
#mkField ChannelLayout, AVChannelLayout
#mkField FrameSize, CInt
#mkField FrameRate, AVRational
#mkField Codec, AVCodec

#hasField AVCodecContext, BitRate, bit_rate
#hasField AVCodecContext, Codec, codec
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
#hasField AVCodecContext, ChannelLayout, ch_layout
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

foreign import ccall "avcodec_parameters_to_context"
  avcodec_parameters_to_context :: AVCodecContext
                                  -> AVCodecParameters
                                  -> IO CInt

newtype AVPacketSideData = AVPacketSideData (Ptr ()) deriving (Storable, HasPtr)

#mkField PacketSideDataData, (Ptr ())
#mkField PacketSideDataSize, CLong
#mkField PacketSideDataType, AVPacketSideDataType

#hasField AVPacketSideData, PacketSideDataData, data
#hasField AVPacketSideData, PacketSideDataSize, size
#hasField AVPacketSideData, PacketSideDataType, type

newtype AVStream = AVStream (Ptr ()) deriving (Storable, HasPtr)
#mkField Id, CInt
#mkField CodecContext, AVCodecContext
#mkField StreamIndex, CInt
#mkField CodecParams, AVCodecParameters
#mkField Dictionary, AVDictionary
#mkField SideData, (Ptr (AVPacketSideData))
#mkField NbSideData, CInt

-- Update this to include side data & metadata in the structure

#hasField AVStream, Id, id
#hasField AVStream, TimeBase, time_base
#hasField AVStream, StreamIndex, index
#hasField AVStream, CodecParams, codecpar
#hasField AVStream, Dictionary, metadata
#hasField AVStream, SideData, side_data
#hasField AVStream, NbSideData, nb_side_data

newtype AVCodec = AVCodec (Ptr ()) deriving (Storable, HasPtr)
#mkField LongName, CString
#mkField Name, CString
#mkField PixelFormats, (Ptr AVPixelFormat)
#mkField SampleFormats, (Ptr AVSampleFormat)
#mkField ChannelLayouts, (Ptr AVChannelLayout)
#mkField SupportedSampleRates, (Ptr CInt)
#mkField Capabilities, CInt

#hasField AVCodec, LongName, long_name
#hasField AVCodec, Name, name
#hasField AVCodec, CodecID, id
#hasField AVCodec, PixelFormats, pix_fmts
#hasField AVCodec, SampleFormats, sample_fmts
#hasField AVCodec, ChannelLayouts, ch_layouts
#hasField AVCodec, SupportedSampleRates, supported_samplerates
#hasField AVCodec, Capabilities, capabilities

newtype AVDictionaryEntry = AVDictionaryEntry (Ptr ()) deriving (Storable, HasPtr)
#mkField Key, CString
#mkField Value, CString

#hasField AVDictionaryEntry, Key, key
#hasField AVDictionaryEntry, Value, value


-- Use av_dict_get and av_dict_set to actually access this structure
newtype AVDictionary = AVDictionary (Ptr ()) deriving (Storable, HasPtr)
  
newtype AVFrame = AVFrame (Ptr ()) deriving (Storable, HasPtr)
#mkField Pts, CLong
#mkField PktDts, CLong
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
#hasField AVFrame, PktDts, pkt_dts
#hasField AVFrame, Data, data
#hasField AVFrame, ExtendedData, extended_data
#hasField AVFrame, NumSamples, nb_samples
#hasField AVFrame, Format, format
#hasField AVFrame, ChannelLayout, ch_layout
#hasField AVFrame, SampleRate, sample_rate

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

newtype AVChannelCustom = AVChannelCustom (Ptr ()) deriving (Storable, HasPtr)

data AVChannelLayout = 
  AVChannelLayout { order :: AVChannelOrder
                  , numChannels :: CInt
                  , mask :: Either CUInt AVChannelCustom                    
                  } -- Ignore the union for now

instance Storable AVChannelLayout where
  sizeOf _ = #size AVChannelLayout
  alignment _ = #size AVChannelLayout
  peek ptr = do
    ord <- (#peek AVChannelLayout, order) ptr
    nc <- (#peek AVChannelLayout, nb_channels) ptr
    maskCust <- if ord == avChannelOrderCustom then do
        custPtr <- (#peek AVChannelLayout, u) ptr
        Right <$> (AVChannelCustom <$> peek custPtr) 
      else
        Left <$> (fromIntegral <$> peekULong)
    pure (AVChannelLayout ord nc maskCust)
    where
      peekULong :: IO CUInt
      peekULong = (#peek AVChannelLayout, u) ptr
 
  poke ptr (AVChannelLayout ord nc maskCustom) = do 
    (#poke AVChannelLayout, order) ptr ord
    (#poke AVChannelLayout, nb_channels) ptr nc
    case maskCustom of
      Left msk -> (#poke AVChannelLayout, u) ptr msk
      Right custom -> (#poke AVChannelLayout, u) ptr custom
                               

foreign import ccall "av_channel_layout_default"
  av_channel_layout_default :: Ptr () -> CInt -> IO ()

channelLayoutDefault :: Ptr () -> CInt -> IO AVChannelLayout
channelLayoutDefault ptr chans = do
  av_channel_layout_default ptr chans
  peek (castPtr ptr)

sizeOfAVChannelLayout :: Int
sizeOfAVChannelLayout = #size AVChannelLayout


cAV_CHANNEL_LAYOUT_MASK :: CInt -> CUInt -> AVChannelLayout
cAV_CHANNEL_LAYOUT_MASK nb  m = AVChannelLayout avChannelOrderNative nb (Left m)

cAV_CHANNEL_LAYOUT_MONO               :: AVChannelLayout
cAV_CHANNEL_LAYOUT_STEREO             :: AVChannelLayout
cAV_CHANNEL_LAYOUT_2POINT1            :: AVChannelLayout
cAV_CHANNEL_LAYOUT_2_1                :: AVChannelLayout
cAV_CHANNEL_LAYOUT_SURROUND           :: AVChannelLayout
cAV_CHANNEL_LAYOUT_3POINT1            :: AVChannelLayout
cAV_CHANNEL_LAYOUT_4POINT0            :: AVChannelLayout
cAV_CHANNEL_LAYOUT_4POINT1            :: AVChannelLayout
cAV_CHANNEL_LAYOUT_2_2                :: AVChannelLayout
cAV_CHANNEL_LAYOUT_QUAD               :: AVChannelLayout
cAV_CHANNEL_LAYOUT_5POINT0            :: AVChannelLayout
cAV_CHANNEL_LAYOUT_5POINT1            :: AVChannelLayout
cAV_CHANNEL_LAYOUT_5POINT0_BACK       :: AVChannelLayout
cAV_CHANNEL_LAYOUT_5POINT1_BACK       :: AVChannelLayout
cAV_CHANNEL_LAYOUT_6POINT0            :: AVChannelLayout
cAV_CHANNEL_LAYOUT_6POINT0_FRONT      :: AVChannelLayout
cAV_CHANNEL_LAYOUT_HEXAGONAL          :: AVChannelLayout
cAV_CHANNEL_LAYOUT_6POINT1            :: AVChannelLayout
cAV_CHANNEL_LAYOUT_6POINT1_BACK       :: AVChannelLayout
cAV_CHANNEL_LAYOUT_6POINT1_FRONT      :: AVChannelLayout
cAV_CHANNEL_LAYOUT_7POINT0            :: AVChannelLayout
cAV_CHANNEL_LAYOUT_7POINT0_FRONT      :: AVChannelLayout
cAV_CHANNEL_LAYOUT_7POINT1            :: AVChannelLayout
cAV_CHANNEL_LAYOUT_7POINT1_WIDE       :: AVChannelLayout
cAV_CHANNEL_LAYOUT_7POINT1_WIDE_BACK  :: AVChannelLayout
cAV_CHANNEL_LAYOUT_OCTAGONAL          :: AVChannelLayout
cAV_CHANNEL_LAYOUT_HEXADECAGONAL      :: AVChannelLayout
cAV_CHANNEL_LAYOUT_STEREO_DOWNMIX     :: AVChannelLayout
cAV_CHANNEL_LAYOUT_22POINT2           :: AVChannelLayout
cAV_CHANNEL_LAYOUT_AMBISONIC_FIRST_ORDER :: AVChannelLayout
cAV_CHANNEL_LAYOUT_MONO              = cAV_CHANNEL_LAYOUT_MASK 1  cAV_CH_LAYOUT_MONO
cAV_CHANNEL_LAYOUT_STEREO            = cAV_CHANNEL_LAYOUT_MASK 2  cAV_CH_LAYOUT_STEREO
cAV_CHANNEL_LAYOUT_2POINT1           = cAV_CHANNEL_LAYOUT_MASK 3  cAV_CH_LAYOUT_2POINT1
cAV_CHANNEL_LAYOUT_2_1               = cAV_CHANNEL_LAYOUT_MASK 3  cAV_CH_LAYOUT_2_1
cAV_CHANNEL_LAYOUT_SURROUND          = cAV_CHANNEL_LAYOUT_MASK 3  cAV_CH_LAYOUT_SURROUND
cAV_CHANNEL_LAYOUT_3POINT1           = cAV_CHANNEL_LAYOUT_MASK 4  cAV_CH_LAYOUT_3POINT1
cAV_CHANNEL_LAYOUT_4POINT0           = cAV_CHANNEL_LAYOUT_MASK 4  cAV_CH_LAYOUT_4POINT0
cAV_CHANNEL_LAYOUT_4POINT1           = cAV_CHANNEL_LAYOUT_MASK 5  cAV_CH_LAYOUT_4POINT1
cAV_CHANNEL_LAYOUT_2_2               = cAV_CHANNEL_LAYOUT_MASK 4  cAV_CH_LAYOUT_2_2
cAV_CHANNEL_LAYOUT_QUAD              = cAV_CHANNEL_LAYOUT_MASK 4  cAV_CH_LAYOUT_QUAD
cAV_CHANNEL_LAYOUT_5POINT0           = cAV_CHANNEL_LAYOUT_MASK 5  cAV_CH_LAYOUT_5POINT0
cAV_CHANNEL_LAYOUT_5POINT1           = cAV_CHANNEL_LAYOUT_MASK 6  cAV_CH_LAYOUT_5POINT1
cAV_CHANNEL_LAYOUT_5POINT0_BACK      = cAV_CHANNEL_LAYOUT_MASK 5  cAV_CH_LAYOUT_5POINT0_BACK
cAV_CHANNEL_LAYOUT_5POINT1_BACK      = cAV_CHANNEL_LAYOUT_MASK 6  cAV_CH_LAYOUT_5POINT1_BACK
cAV_CHANNEL_LAYOUT_6POINT0           = cAV_CHANNEL_LAYOUT_MASK 6  cAV_CH_LAYOUT_6POINT0
cAV_CHANNEL_LAYOUT_6POINT0_FRONT     = cAV_CHANNEL_LAYOUT_MASK 6  cAV_CH_LAYOUT_6POINT0_FRONT
cAV_CHANNEL_LAYOUT_HEXAGONAL         = cAV_CHANNEL_LAYOUT_MASK 6  cAV_CH_LAYOUT_HEXAGONAL
cAV_CHANNEL_LAYOUT_6POINT1           = cAV_CHANNEL_LAYOUT_MASK 7  cAV_CH_LAYOUT_6POINT1
cAV_CHANNEL_LAYOUT_6POINT1_BACK      = cAV_CHANNEL_LAYOUT_MASK 7  cAV_CH_LAYOUT_6POINT1_BACK
cAV_CHANNEL_LAYOUT_6POINT1_FRONT     = cAV_CHANNEL_LAYOUT_MASK 7  cAV_CH_LAYOUT_6POINT1_FRONT
cAV_CHANNEL_LAYOUT_7POINT0           = cAV_CHANNEL_LAYOUT_MASK 7  cAV_CH_LAYOUT_7POINT0
cAV_CHANNEL_LAYOUT_7POINT0_FRONT     = cAV_CHANNEL_LAYOUT_MASK 7  cAV_CH_LAYOUT_7POINT0_FRONT
cAV_CHANNEL_LAYOUT_7POINT1           = cAV_CHANNEL_LAYOUT_MASK 8  cAV_CH_LAYOUT_7POINT1
cAV_CHANNEL_LAYOUT_7POINT1_WIDE      = cAV_CHANNEL_LAYOUT_MASK 8  cAV_CH_LAYOUT_7POINT1_WIDE
cAV_CHANNEL_LAYOUT_7POINT1_WIDE_BACK = cAV_CHANNEL_LAYOUT_MASK 8  cAV_CH_LAYOUT_7POINT1_WIDE_BACK
cAV_CHANNEL_LAYOUT_OCTAGONAL         = cAV_CHANNEL_LAYOUT_MASK 8  cAV_CH_LAYOUT_OCTAGONAL
cAV_CHANNEL_LAYOUT_HEXADECAGONAL     = cAV_CHANNEL_LAYOUT_MASK 16 cAV_CH_LAYOUT_HEXADECAGONAL
cAV_CHANNEL_LAYOUT_STEREO_DOWNMIX    = cAV_CHANNEL_LAYOUT_MASK 2  cAV_CH_LAYOUT_STEREO_DOWNMIX
cAV_CHANNEL_LAYOUT_22POINT2          = cAV_CHANNEL_LAYOUT_MASK 24 cAV_CH_LAYOUT_22POINT2
cAV_CHANNEL_LAYOUT_AMBISONIC_FIRST_ORDER = AVChannelLayout avChannelOrderAmbisonic 4 (Left 0)
