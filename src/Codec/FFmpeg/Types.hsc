{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances,
             GeneralizedNewtypeDeriving #-}
module Codec.FFmpeg.Types where
import Codec.FFmpeg.Enums
import Control.Applicative
import Foreign.C.String (CString)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/avutil.h>
#include <libswscale/swscale.h>
#include "hscMacros.h"

class HasPtr a where
  getPtr :: a -> Ptr ()

instance HasPtr (Ptr ()) where getPtr = id

newtype AVFormatContext = AVFormatContext (Ptr ()) deriving (Storable, HasPtr)
#mkField NumStreams, CInt
#mkField Streams, (Ptr AVStream)
#mkField OutputFormat, AVOutputFormat
#mkField IOContext, AVIOContext

#hasField AVFormatContext, NumStreams, nb_streams
#hasField AVFormatContext, Streams, streams
#hasField AVFormatContext, OutputFormat, oformat
#hasField AVFormatContext, IOContext, pb

newtype AVCodecContext = AVCodecContext (Ptr ()) deriving (Storable, HasPtr)

#mkField BitRate, CInt
#hasField AVCodecContext, BitRate, bit_rate

#mkField Width, CInt
#mkField Height, CInt
#mkField TimeBase, AVRational
#mkField GopSize, CInt
#mkField PixelFormat, AVPixelFormat
#mkField CodecFlags, CodecFlag
#mkField CodecID, AVCodecID
#mkField PrivData, (Ptr ())

#hasField AVCodecContext, Width, width
#hasField AVCodecContext, Height, height
#hasField AVCodecContext, TimeBase, time_base
#hasField AVCodecContext, GopSize, gop_size
#hasField AVCodecContext, PixelFormat, pix_fmt
#hasField AVCodecContext, CodecFlags, flags
#hasField AVCodecContext, CodecID, codec_id
#hasField AVCodecContext, PrivData, priv_data

newtype AVStream = AVStream (Ptr ()) deriving (Storable, HasPtr)

#mkField Id, CInt
#mkField CodecContext, AVCodecContext
#mkField StreamIndex, CInt

#hasField AVStream, Id, id
#hasField AVStream, TimeBase, time_base
#hasField AVStream, CodecContext, codec
#hasField AVStream, StreamIndex, index

newtype AVCodec = AVCodec (Ptr ()) deriving (Storable, HasPtr)
#mkField LongName, CString
#mkField Name, CString
#mkField PixelFormats, (Ptr AVPixelFormat)

#hasField AVCodec, LongName, long_name
#hasField AVCodec, Name, name
#hasField AVCodec, CodecID, id
#hasField AVCodec, PixelFormats, pix_fmts

newtype AVDictionary = AVDictionary (Ptr ()) deriving (Storable, HasPtr)
newtype AVFrame = AVFrame (Ptr ()) deriving (Storable, HasPtr)
#mkField Pts, CLong
#mkField PktPts, CLong
#mkField LineSize, CInt

#hasField AVFrame, PixelFormat, format
#hasField AVFrame, Width, width
#hasField AVFrame, Height, height
#hasField AVFrame, LineSize, linesize
#hasField AVFrame, Pts, pts
#hasField AVFrame, PktPts, pkt_pts
#hasField AVFrame, Data, data

newtype AVPicture = AVPicture (Ptr ()) deriving (Storable, HasPtr)
#hasField AVPicture, Data, data

newtype SwsContext = SwsContext (Ptr ()) deriving (Storable, HasPtr)
newtype AVOutputFormat = AVOutputFormat (Ptr ()) deriving (Storable, HasPtr)
#mkField FormatFlags, FormatFlag
#mkField VideoCodecID, AVCodecID
#hasField AVOutputFormat, FormatFlags, flags
#hasField AVOutputFormat, VideoCodecID, video_codec

newtype AVIOContext = AVIOContext (Ptr ()) deriving (Storable, HasPtr)

newtype AVPacket = AVPacket (Ptr ()) deriving (Storable, HasPtr)
#mkField Data, (Ptr ())
#mkField Size, CInt
#mkField PacketFlags, PacketFlag
#mkField Dts, CLong

#hasField AVPacket, Data, data
#hasField AVPacket, Size, size
#hasField AVPacket, PacketFlags, flags
#hasField AVPacket, StreamIndex, stream_index
#hasField AVPacket, Pts, pts
#hasField AVPacket, Dts, dts

-- | @sizeof@ the 'AVPacket' structure in bytes.
packetSize :: Int
packetSize = #size AVPacket

pictureSize :: Int
pictureSize = #size AVPicture

-- * Types with Haskell equivalents

data AVRational = AVRational { numerator   :: CInt
                             , denomenator :: CInt } deriving Show

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
