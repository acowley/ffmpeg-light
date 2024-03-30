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
import Codec.FFmpeg.Types
import Codec.Picture
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C.Types
import Foreign.Storable (sizeOf)
import Codec.FFmpeg.Display (DisplayRotationDegrees)
import Codec.Picture.Extra (rotate180, rotateRight90, rotateLeft90)

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
-- | Rotate it if display rotation is available as side data
toJuicyImage :: forall p. (JuicyPixelFormat p) => Bool -> Maybe DisplayRotationDegrees -> AVFrame -> IO (Maybe (Image p))
toJuicyImage rotateIfPres mdisp frame = runMaybeT $ do
  fmt <- lift $ getPixelFormat frame
  guard (fmt == juicyPixelFormat ([] :: [p]))
  w <- lift $ fromIntegral <$> getWidth frame
  h <- lift $ fromIntegral <$> getHeight frame

  img <- MaybeT $ fmap (Image w h . V.unsafeCast) <$> frameToVector frame
  maybe (pure img) (pure . rotate img) (if rotateIfPres then mdisp else Nothing)

rotate :: forall p. (JuicyPixelFormat p) =>  Image p  -> DisplayRotationDegrees -> Image p
rotate img rotation
  | abs rotation >= 180 = rotate180 img
  | rotation == (-90) = rotateRight90 img
  | rotation == 90 = rotateLeft90 img
  -- TODO handle this exception case
  | otherwise = img 


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

-- | Read frames from a video stream.
imageReaderT ::
  forall m p.
  ( Functor m,
    MonadIO m,
    MonadError String m,
    JuicyPixelFormat p
  ) =>
  Bool -> 
  InputSource ->
  m (IO (Maybe (Image p)), IO (), VideoStreamMetadata)
imageReaderT mdisp is = do
  (r, c, md) <- frameReader (juicyPixelFormat ([] :: [p])) is
  pure (aux md r, c, md)
  -- fmap (first3 (runMaybeT . aux (toJuicyImage mdisp)))
  --   . frameReader (juicyPixelFormat ([] :: [p]))
  where
    aux md r = runMaybeT $ do
      frame <- MaybeT r
      MaybeT $ toJuicyImage mdisp (displayRotation md) frame

-- | Read frames from a video stream. Errors are thrown as
-- 'IOException's.
imageReader ::
  (JuicyPixelFormat p) =>
  Bool ->
  InputSource ->
  IO (IO (Maybe (Image p)), IO (), VideoStreamMetadata)
imageReader mdisp = either error return <=< (runExceptT . imageReaderT mdisp)

-- | Read time stamped frames from a video stream. Time is given in
-- seconds from the start of the stream.
imageReaderTimeT ::
  forall m p.
  ( Functor m,
    MonadIO m,
    MonadError String m,
    JuicyPixelFormat p
  ) =>
  Bool ->
  InputSource ->
  m (IO (Maybe (Image p, Double)), IO (), VideoStreamMetadata)
imageReaderTimeT mdisp is = do
  (r, c, md) <- frameReaderTime (juicyPixelFormat ([] :: [p])) is
  pure (aux md r, c, md)
  where
    aux md r = runMaybeT $ do
      (frame, ts) <- MaybeT r
      frame' <- MaybeT $ toJuicyImage mdisp (displayRotation md) frame
      return (frame', ts)

-- | Read time stamped frames from a video stream. Time is given in
-- seconds from the start of the stream. Errors are thrown as
-- 'IOException's.
imageReaderTime ::
  (JuicyPixelFormat p) =>
  Bool ->
  InputSource ->
  IO (IO (Maybe (Image p, Double)), IO (), VideoStreamMetadata)
imageReaderTime mdisp = either error return <=< runExceptT . imageReaderTimeT mdisp

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
