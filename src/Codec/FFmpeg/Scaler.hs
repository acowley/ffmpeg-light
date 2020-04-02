{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables,
             UndecidableInstances #-}
module Codec.FFmpeg.Scaler where
import Codec.FFmpeg.Common
import Codec.FFmpeg.Enums
import Codec.FFmpeg.Internal.Linear (V2(..))
import Codec.FFmpeg.Types
import Codec.Picture
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Storable as V
import Foreign.C.Types
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (castPtr, nullPtr, Ptr)
import Foreign.Storable (Storable(sizeOf))

data ImageInfo = ImageInfo { imgWidth  :: CInt
                           , imgHeight :: CInt
                           , imgFormat :: AVPixelFormat }

-- | @swsInit srcInfo dstInfo alg@ initializations an 'SwsContext' to
-- scale and convert from @srcInfo@ to @dstInfo@ using the algorithm
-- @alg@ when scaling.
swsInit :: ImageInfo -> ImageInfo -> SwsAlgorithm -> IO SwsContext
swsInit = swsReset (SwsContext nullPtr)

-- | Obtain a context for converting the source to destination
-- format. If the given context is already configured for the required
-- conversion, it is returned. Otherwise, the given context is freed
-- and a new, configured context is returned. See 'swsInit' for a
-- description of the arguments.
swsReset :: SwsContext -> ImageInfo -> ImageInfo -> SwsAlgorithm
         -> IO SwsContext
swsReset ctx src dst alg = sws_getCachedContext ctx
                             srcW srcH srcFmt
                             dstW dstH dstFmt
                             alg nullPtr nullPtr nullPtr
  where ImageInfo srcW srcH srcFmt = src
        ImageInfo dstW dstH dstFmt = dst

-- | A common interface required of arguments to 'swsScale' (a higher
-- level wrapper for the 'sws_scale' function from @libswscale@).
class SwsCompatible a where
  swsPlanes :: a -> (Ptr (Ptr CUChar) -> IO r) -> IO r
  swsStrides :: a -> (Ptr CInt -> IO r) -> IO r
  sliceHeight :: a -> (CInt -> IO r) -> IO r

instance SwsCompatible AVFrame where
  swsPlanes frame k = k (castPtr $ hasFrameData frame)
  swsStrides frame k = k (hasLineSize frame)
  sliceHeight frame k = getHeight frame >>= k

instance (Pixel a, Storable (PixelBaseComponent a))
  => SwsCompatible (Image a) where
  swsPlanes img k = V.unsafeWith (imageData img) $ \ptr ->
                      withArray (castPtr ptr : replicate 7 nullPtr) k
  swsStrides img k = withArray (stride : replicate 7 0) k
    where sz = sizeOf (undefined::PixelBaseComponent a) * 
               componentCount (undefined :: a)
          stride = fromIntegral $ imageWidth img * sz
  sliceHeight img k = k (fromIntegral $ imageHeight img)

instance SwsCompatible (AVPixelFormat, V2 CInt, V.Vector CUChar) where
  swsPlanes (_,_,p) k = V.unsafeWith p $ \ptr ->
                          withArray (castPtr ptr : replicate 7 nullPtr) k
  swsStrides (fmt, V2 w _, _) k = withArray (stride : replicate 7 0) k
    where sz = fromMaybe (error $ "Unknown pixel stride for format "++show fmt)
                         (avPixelStride fmt)
          stride = w * fromIntegral sz
  sliceHeight (_, V2 _ h, _) k = k h

-- | Supplies a continuation with all components provided by the
-- 'SwsCompatible' class.
withSws :: SwsCompatible a
        => a -> (Ptr (Ptr CUChar) -> Ptr CInt -> CInt -> IO r) -> IO r
withSws img k = swsPlanes img $ \planes ->
                  swsStrides img $ \strides ->
                    sliceHeight img $ \height ->
                      k planes strides height
                        
-- | @swsScale ctx src dst@ scales the entire @src@ image to @dst@
-- using the previously initialized @ctx@.
swsScale :: (SwsCompatible src, SwsCompatible dst)
         => SwsContext -> src -> dst -> IO CInt
swsScale ctx src dst = withSws src $ \srcPlanes srcStrides srcHeight ->
                         withSws dst $ \dstPlanes dstStrides _ ->
                           sws_scale ctx srcPlanes srcStrides
                                     0 srcHeight
                                     dstPlanes dstStrides
