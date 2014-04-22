{-# LANGUAGE FlexibleContexts, TypeSynonymInstances #-}
-- | Convert between FFmpeg frames and JuicyPixels images.
module Codec.FFmpeg.Juicy where
import Codec.Picture
import Codec.FFmpeg.Enums
import Codec.FFmpeg.Types
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad (when, (>=>))
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C.Types
import Foreign.Marshal.Array (advancePtr, copyArray)
import Foreign.Ptr (castPtr, Ptr)

-- | Convert an 'AVFrame' to a 'DynamicImage' with the result in the
-- 'MaybeT' transformer.
-- 
-- > toJuicyT = MaybeT . toJuicy
toJuicyT :: AVFrame -> MaybeT IO DynamicImage
toJuicyT = MaybeT . toJuicy

-- | Convert an 'AVFrame' to a 'DynamicImage'.
toJuicy :: AVFrame -> IO (Maybe DynamicImage)
toJuicy frame = do
  fmt <- getPixelFormat frame
  when (fmt /= avPixFmtRgb24)
       (putStrLn "Not RGB24?!")
  w <- fromIntegral <$> getWidth frame
  h <- fromIntegral <$> getHeight frame
  pixels <- castPtr <$> getData frame :: IO (Ptr CUChar)
  srcStride <- fromIntegral <$> getLineSize frame
  let dstStride = w * 3
  v <- VM.new (w*h*3)
  VM.unsafeWith v $ \vptr ->
    mapM_ (\(i,o) -> copyArray (advancePtr vptr o)
                               (advancePtr pixels i)
                               (w * 3))
          (map ((srcStride *) &&& (dstStride*)) [0 .. h - 1])
  v' <- V.unsafeFreeze v
  let mkImage :: V.Storable (PixelBaseComponent a)
              => (Image a -> DynamicImage) -> Maybe DynamicImage
      mkImage c = Just $ c (Image w h (V.unsafeCast v'))
  return $
    case () of
      _ | fmt == avPixFmtRgb24 -> mkImage ImageRGB8
        | fmt == avPixFmtGray8 -> mkImage ImageY8
        | fmt == avPixFmtGray16 -> mkImage ImageY16
        | otherwise -> Nothing

-- | Save an 'AVFrame' to a PNG file on disk assuming the frame could
-- be converted to a 'DynamicImage' using 'toJuicy'.
saveJuicy :: FilePath -> AVFrame -> IO ()
saveJuicy name = toJuicy >=> traverse_ (savePngImage name)

-- | Mapping of @JuicyPixels@ pixel types to FFmpeg pixel formats.
class JuicyPixelFormat a where
  juicyPixelFormat :: proxy a -> AVPixelFormat

instance JuicyPixelFormat Pixel8 where
  juicyPixelFormat _ = avPixFmtGray8

instance JuicyPixelFormat PixelRGB8 where
  juicyPixelFormat _ = avPixFmtRgb24

instance JuicyPixelFormat PixelRGBA8 where
  juicyPixelFormat _ = avPixFmtRgba
