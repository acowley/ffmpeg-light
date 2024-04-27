module Codec.FFmpeg.AudioStream where

import           Codec.FFmpeg.Enums
import           Data.Bits
import           Foreign.C.Types
import Codec.FFmpeg.Types (AVChannelLayout)

data AudioStream = AudioStream
  { asBitRate       :: CInt
  , asSampleFormat  :: AVSampleFormat
  , asSampleRate    :: CInt
  , asChannelLayout :: AVChannelLayout
  , asChannelCount  :: CInt
  , asCodec         :: AVCodecID
  }

-- These are all defined as #defines so I don't think we can FFI them
avChFrontLeft :: CULong
avChFrontLeft = 0x00000001

avChFrontRight :: CULong
avChFrontRight = 0x00000002

avChFrontCenter :: CULong
avChFrontCenter = 0x00000004

avChLowFrequency :: CULong
avChLowFrequency = 0x00000008

avChBackLeft :: CULong
avChBackLeft = 0x00000010

avChBackRight :: CULong
avChBackRight = 0x00000020

avChFrontLeftOfCenter :: CULong
avChFrontLeftOfCenter = 0x00000040

avChFrontRightOfCenter :: CULong
avChFrontRightOfCenter =  0x00000080

avChBackCenter :: CULong
avChBackCenter = 0x00000100

avChSideLeft :: CULong
avChSideLeft = 0x00000200

avChSideRight :: CULong
avChSideRight = 0x00000400

avChTopCenter :: CULong
avChTopCenter = 0x00000800

avChTopFrontLeft :: CULong
avChTopFrontLeft = 0x00001000

avChTopFrontCenter :: CULong
avChTopFrontCenter = 0x00002000

avChTopFrontRight :: CULong
avChTopFrontRight = 0x00004000

avChTopBackLeft :: CULong
avChTopBackLeft = 0x00008000

avChTopBackCenter :: CULong
avChTopBackCenter = 0x00010000

avChTopBackRight :: CULong
avChTopBackRight = 0x00020000

avChStereoLeft :: CULong
avChStereoLeft = 0x20000000

avChStereoRight :: CULong
avChStereoRight = 0x40000000

avChLayoutMono =              avChFrontCenter
avChLayoutStereo =            avChFrontLeft .|. avChFrontRight
avChLayout2point1 =           avChLayoutStereo .|. avChLowFrequency
avChLayout21 =               avChLayoutStereo .|. avChBackCenter
avChLayoutSurround =          avChLayoutStereo .|. avChFrontCenter
avChLayout3point1 =           avChLayoutSurround .|. avChLowFrequency
avChLayout4point0 =           avChLayoutSurround .|. avChBackCenter
avChLayout4point1 =           avChLayout4point0 .|. avChLowFrequency
avChLayout22 =               avChLayoutStereo .|. avChSideLeft .|. avChSideRight
avChLayoutQuad =              avChLayoutStereo .|. avChBackLeft .|. avChBackRight
avChLayout5point0 =           avChLayoutSurround .|. avChSideLeft .|. avChSideRight
avChLayout5point1 =           avChLayout5point0 .|. avChLowFrequency
avChLayout5point0Back =      avChLayoutSurround .|. avChBackLeft .|. avChBackRight
avChLayout5point1Back =      avChLayout5point0Back .|. avChLowFrequency
avChLayout6point0 =           avChLayout5point0 .|. avChBackCenter
avChLayout6point0Front =     avChLayout22 .|. avChFrontLeftOfCenter .|. avChFrontRightOfCenter
avChLayoutHexagonal =         avChLayout5point0Back .|. avChBackCenter
avChLayout6point1 =           avChLayout5point1 .|. avChBackCenter
avChLayout6point1Back =      avChLayout5point1Back .|. avChBackCenter
avChLayout6point1Front =     avChLayout6point0Front .|. avChLowFrequency
avChLayout7point0 =           avChLayout5point0 .|. avChBackLeft .|. avChBackRight
avChLayout7point0Front =     avChLayout5point0 .|. avChFrontLeftOfCenter .|. avChFrontRightOfCenter
avChLayout7point1 =           avChLayout5point1 .|. avChBackLeft .|. avChBackRight
-- avChLayout7point1Wide =      avChLayout5point1 .|. avChFrontLeftOfCenter .|. avChFrontRightOfCenter
-- avChLayout7point1WideBack = avChLayout5point1Back .|. avChFrontLeftOfCenter .|. avChFrontRightOfCenter
avChLayoutOctagonal =         avChLayout5point0 .|. avChBackLeft .|. avChBackCenter .|. avChBackRight
-- avChLayoutHexadecagonal =     avChLayoutOctagonal .|. avChWideLeft .|. avChWideRight .|. avChTopBackLeft .|. avChTopBackRight .|. avChTopBackCenter .|. avChTopFrontCenter .|. avChTopFrontLeft .|. avChTopFrontRight
avChLayoutStereoDownmix =    avChStereoLeft .|. avChStereoRight

