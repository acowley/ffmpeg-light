module Codec.FFmpeg.AudioStream where

import           Codec.FFmpeg.Enums
import qualified Data.Vector.Storable as V
import           Foreign.C.Types

data AudioStream = AudioStream
  { asBitRate       :: CInt
  , asSampleFormat  :: AVSampleFormat
  , asSampleRate    :: CInt
  , asChannelLayout :: CULong
  , asChannelCount  :: CInt
  , asCodec         :: AVCodecID
  }

data AudioFrame =
    U8 (V.Vector CUChar)
  | S16 (V.Vector CShort)
  | S32 (V.Vector CInt)
  | FLT (V.Vector CFloat)
  | DBL (V.Vector CDouble)
  | U8P (V.Vector CUChar)
  | S16P (V.Vector CShort)
  | S32P (V.Vector CInt)
  | FLTP (V.Vector CFloat)
  | DBLP (V.Vector CDouble)
