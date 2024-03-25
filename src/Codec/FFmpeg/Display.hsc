{-# LANGUAGE ForeignFunctionInterface, FlexibleInstances,
             GeneralizedNewtypeDeriving #-}

module Codec.FFmpeg.Display where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Types
import Foreign.Marshal.Array (newArray)
import Data.Int (Int32)

import Codec.FFmpeg.Types (AVPacketSideData (..), getPacketSideDataData, AVStream (..), getPacketSideDataType)
import Codec.FFmpeg.Enums (AVPacketSideDataType (..), avPktDataDisplaymatrix)
import Codec.FFmpeg.Common (av_malloc)

#include <math.h>
#include <libavutil/display.h>

foreign import ccall unsafe "isnan"
  isnan    :: CDouble -> CInt

-- double av_display_rotation_get(const int32_t matrix[9]);

foreign import ccall "av_display_rotation_get"
  av_display_rotation_get :: Ptr () -> IO CDouble

type DisplayRotationDegrees = Integer

getDisplayRotation :: AVPacketSideData -> IO (Maybe DisplayRotationDegrees)
getDisplayRotation avp = do
  case getPacketSideDataType avp of 
    avPktDataDisplaymatrix -> do 
      ptr <- getPacketSideDataData avp
      rot <- av_display_rotation_get ptr
      pure $ if isnan rot > 0 then Nothing else Just (round rot)
    _ -> pure Nothing

displayRotationCSize :: CSize
displayRotationCSize = fromIntegral (sizeOf (1::CInt) * 9)

newtype DisplayRotation = DisplayRotation (Ptr ()) deriving (Storable)

foreign import ccall "av_display_rotation_set"
  av_display_rotation_set :: DisplayRotation -> CDouble -> IO ()

setDisplayRotation :: Double -> IO DisplayRotation
setDisplayRotation angle = do
  arr <- DisplayRotation <$> av_malloc displayRotationCSize
  av_display_rotation_set arr (CDouble angle)
  pure arr

foreign import ccall "av_stream_add_side_data"
  av_stream_add_side_data :: AVStream -> AVPacketSideDataType -> Ptr () -> CSize -> IO ()

addAsSideData :: AVStream -> DisplayRotation -> IO ()
addAsSideData avs (DisplayRotation ptr) = av_stream_add_side_data avs avPktDataDisplaymatrix (castPtr ptr) displayRotationCSize
