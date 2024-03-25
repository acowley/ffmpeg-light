{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Convert between FFmpeg frames and JuicyPixels images.
module Codec.FFmpeg.Juicy where

import Codec.FFmpeg.Common
import Codec.FFmpeg.Decode
import Codec.FFmpeg.Encode
import Codec.FFmpeg.Enums
import Codec.FFmpeg.Internal.Linear (V2 (..))
import Codec.FFmpeg.Probe
import Codec.FFmpeg.Types
import Codec.Picture
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty, singleton, (<|))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C.Types
import Foreign.Storable (sizeOf)

-- | Convert 'AVFrame' to a 'Vector'.
frameToVector :: AVFrame -> IO (Maybe (V.Vector CUChar))
frameToVector = runMaybeT . frameToVectorT

-- | Convert 'AVFrame' to a 'Vector' with the result in the 'MaybeT' transformer.
frameToVectorT :: AVFrame -> MaybeT IO (V.Vector CUChar)
frameToVectorT frame = do
  bufSize <- fromIntegral <$> frameBufferSizeT frame

  v <- MaybeT $ do
    v <- VM.new bufSize

    VM.unsafeWith v (frameCopyToBuffer frame)
      >>= return . maybe Nothing (const (Just v))

  lift $ V.unsafeFreeze v

-- | Convert an 'AVFrame' to a 'DynamicImage' with the result in the
-- 'MaybeT' transformer.
--
-- > toJuicyT = MaybeT . toJuicy
toJuicyT :: AVFrame -> MaybeT IO DynamicImage
toJuicyT = MaybeT . toJuicy

-- | Convert an 'AVFrame' to a 'DynamicImage'.
toJuicy :: AVFrame -> IO (Maybe DynamicImage)
toJuicy frame = runMaybeT $ do
  v <- frameToVectorT frame

  MaybeT $ do
    w <- fromIntegral <$> getWidth frame
    h <- fromIntegral <$> getHeight frame

    let mkImage ::
          (V.Storable (PixelBaseComponent a)) =>
          (Image a -> DynamicImage) ->
          Maybe DynamicImage
        mkImage c = Just $ c (Image w h (V.unsafeCast v))

    fmt <- getPixelFormat frame

    return $ case () of
      _
        | fmt == avPixFmtRgb24 -> mkImage ImageRGB8
        | fmt == avPixFmtGray8 -> mkImage ImageY8
        | fmt == avPixFmtGray16 -> mkImage ImageY16
        | otherwise -> Nothing

-- | Convert an 'AVFrame' to an 'Image'.
toJuicyImage :: forall p. (JuicyPixelFormat p) => AVFrame -> IO (Maybe (Image p))
toJuicyImage frame = runMaybeT $ do
  fmt <- lift $ getPixelFormat frame
  guard (fmt == juicyPixelFormat ([] :: [p]))

  MaybeT $ do
    w <- fromIntegral <$> getWidth frame
    h <- fromIntegral <$> getHeight frame

    fmap (Image w h . V.unsafeCast) <$> frameToVector frame

-- | Save an 'AVFrame' to a PNG file on disk assuming the frame could
-- be converted to a 'DynamicImage' using 'toJuicy'.
saveJuicy :: FilePath -> AVFrame -> IO ()
saveJuicy name = toJuicy >=> traverse_ (savePngImage name)

-- | Mapping of @JuicyPixels@ pixel types to FFmpeg pixel formats.
class (Pixel a) => JuicyPixelFormat a where
  juicyPixelFormat :: proxy a -> AVPixelFormat

instance JuicyPixelFormat Pixel8 where
  juicyPixelFormat _ = avPixFmtGray8

instance JuicyPixelFormat PixelRGB8 where
  juicyPixelFormat _ = avPixFmtRgb24

instance JuicyPixelFormat PixelRGBA8 where
  juicyPixelFormat _ = avPixFmtRgba

-- | Bytes-per-pixel for a JuicyPixels 'Pixel' type.
juicyPixelStride :: forall a proxy. (Pixel a) => proxy a -> Int
juicyPixelStride _ =
  sizeOf (undefined :: PixelBaseComponent a) * componentCount (undefined :: a)

type Metadata = NonEmpty (String, String)

avDictionaryToKV :: (MonadIO m, JuicyPixelFormat p) => m (a1, b, Maybe AVDictionary) -> ((AVFrame -> IO (Maybe (Image p))) -> a1 -> MaybeT m2 a2) -> m (m2 (Maybe a2), b, Maybe Metadata)
avDictionaryToKV f aux = do
  (frame, cleanup, avdict) <- f
  let frameResult = runMaybeT (aux toJuicyImage frame)
  md <- dictFoldM folder Nothing `mapM` avdict
  pure (frameResult, cleanup, join md)
  where
    folder Nothing kv = pure (Just (singleton kv))
    folder (Just nes) kv = pure (Just (kv <| nes))

-- | Read frames from a video stream.
imageReaderT ::
  forall m p.
  ( Functor m,
    MonadIO m,
    MonadError String m,
    JuicyPixelFormat p
  ) =>
  InputSource ->
  m (IO (Maybe (Image p)), IO (), VideoStreamMetadata)
imageReaderT =
  fmap (first3 (runMaybeT . aux toJuicyImage))
    . frameReader (juicyPixelFormat ([] :: [p]))
  where
    aux g x = MaybeT x >>= MaybeT . g

-- | Read frames from a video stream. Errors are thrown as
-- 'IOException's.
imageReader ::
  (JuicyPixelFormat p) =>
  InputSource ->
  IO (IO (Maybe (Image p)), IO (), VideoStreamMetadata)
imageReader = either error return <=< (runExceptT . imageReaderT)

-- | Read time stamped frames from a video stream. Time is given in
-- seconds from the start of the stream.
imageReaderTimeT ::
  forall m p.
  ( Functor m,
    MonadIO m,
    MonadError String m,
    JuicyPixelFormat p
  ) =>
  InputSource ->
  m (IO (Maybe (Image p, Double)), IO (), VideoStreamMetadata)
imageReaderTimeT =
  fmap (first3 (runMaybeT . aux toJuicyImage))
    . frameReaderTime (juicyPixelFormat ([] :: [p]))
  where
    aux g x = do
      (f, t) <- MaybeT x
      f' <- MaybeT $ g f
      return (f', t)

-- | Read time stamped frames from a video stream. Time is given in
-- seconds from the start of the stream. Errors are thrown as
-- 'IOException's.
imageReaderTime ::
  (JuicyPixelFormat p) =>
  InputSource ->
  IO (IO (Maybe (Image p, Double)), IO (), VideoStreamMetadata)
imageReaderTime = either error return <=< runExceptT . imageReaderTimeT

-- | Open a target file for writing a video stream. When the returned
-- function is applied to 'Nothing', the output stream is closed. Note
-- that 'Nothing' /must/ be provided when finishing in order to
-- properly terminate video encoding.
--
-- Support for source images that are of a different size to the
-- output resolution is limited to non-palettized destination formats
-- (i.e. those that are handled by @libswscaler@). Practically, this
-- means that animated gif output is only supported if the source
-- images are of the target resolution.
imageWriter ::
  forall p.
  (JuicyPixelFormat p) =>
  EncodingParams ->
  FilePath ->
  IO (Maybe (Image p) -> IO ())
imageWriter ep f = do
  vw <- videoWriter ep f
  return $ (. fmap fromJuciy) vw

-- | Util function to convert a JuicyPixels image to the same structure
-- used by 'frameWriter'
fromJuciy ::
  forall p.
  (JuicyPixelFormat p) =>
  Image p ->
  (AVPixelFormat, V2 CInt, V.Vector CUChar)
fromJuciy img = (juicyPixelFormat ([] :: [p]), V2 w h, p)
  where
    w = fromIntegral $ imageWidth img
    h = fromIntegral $ imageHeight img
    p = V.unsafeCast $ imageData img
