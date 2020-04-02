{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances #-}
-- | Convert between FFmpeg frames and JuicyPixels images.
module Codec.FFmpeg.Juicy where
import Codec.Picture
import Codec.FFmpeg.Common
import Codec.FFmpeg.Decode
import Codec.FFmpeg.Encode
import Codec.FFmpeg.Enums
import Codec.FFmpeg.Internal.Linear (V2(..))
import Codec.FFmpeg.Types
import Control.Arrow (first)
import Control.Monad ((>=>))
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Foldable (traverse_)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C.Types
import Foreign.Storable (sizeOf)
import Data.Maybe (maybe)


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

    let mkImage :: V.Storable (PixelBaseComponent a)
                => (Image a -> DynamicImage) -> Maybe DynamicImage
        mkImage c = Just $ c (Image w h (V.unsafeCast v))

    fmt <- getPixelFormat frame

    return $ case () of
               _ | fmt == avPixFmtRgb24 -> mkImage ImageRGB8
                 | fmt == avPixFmtGray8 -> mkImage ImageY8
                 | fmt == avPixFmtGray16 -> mkImage ImageY16
                 | otherwise -> Nothing


-- | Convert an 'AVFrame' to an 'Image'.
toJuicyImage :: forall p. JuicyPixelFormat p => AVFrame -> IO (Maybe (Image p))
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
class Pixel a => JuicyPixelFormat a where
  juicyPixelFormat :: proxy a -> AVPixelFormat

instance JuicyPixelFormat Pixel8 where
  juicyPixelFormat _ = avPixFmtGray8

instance JuicyPixelFormat PixelRGB8 where
  juicyPixelFormat _ = avPixFmtRgb24

instance JuicyPixelFormat PixelRGBA8 where
  juicyPixelFormat _ = avPixFmtRgba

-- | Bytes-per-pixel for a JuicyPixels 'Pixel' type.
juicyPixelStride :: forall a proxy. Pixel a => proxy a -> Int
juicyPixelStride _ =
  sizeOf (undefined :: PixelBaseComponent a) * componentCount (undefined :: a)

-- | Read frames from a video stream.
imageReaderT :: forall m p.
                (Functor m, MonadIO m, MonadError String m,
                 JuicyPixelFormat p)
             => InputSource -> m (IO (Maybe (Image p)), IO ())
imageReaderT = fmap (first (runMaybeT . aux toJuicyImage))
            . frameReader (juicyPixelFormat ([] :: [p]))
  where aux g x = MaybeT x >>= MaybeT . g

-- | Read frames from a video stream. Errors are thrown as
-- 'IOException's.
imageReader :: JuicyPixelFormat p
            => InputSource -> IO (IO (Maybe (Image p)), IO ())
imageReader = (>>= either error return) . runExceptT . imageReaderT

-- | Read time stamped frames from a video stream. Time is given in
-- seconds from the start of the stream.
imageReaderTimeT :: forall m p.
                    (Functor m, MonadIO m, MonadError String m,
                     JuicyPixelFormat p)
                 => InputSource -> m (IO (Maybe (Image p, Double)), IO ())
imageReaderTimeT = fmap (first (runMaybeT . aux toJuicyImage))
                 . frameReaderTime (juicyPixelFormat ([] :: [p]))
  where aux g x = do (f,t) <- MaybeT x
                     f' <- MaybeT $ g f
                     return (f', t)

-- | Read time stamped frames from a video stream. Time is given in
-- seconds from the start of the stream. Errors are thrown as
-- 'IOException's.
imageReaderTime :: JuicyPixelFormat p
                => InputSource -> IO (IO (Maybe (Image p, Double)), IO ())
imageReaderTime = (>>= either error return) . runExceptT . imageReaderTimeT

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
imageWriter :: forall p. JuicyPixelFormat p
            => EncodingParams -> FilePath -> IO (Maybe (Image p) -> IO ())
imageWriter ep f = do
  (videoWriter, _) <- frameWriter ep f
  return $ (. fmap aux) videoWriter
  where aux img = let w = fromIntegral $ imageWidth img
                      h = fromIntegral $ imageHeight img
                      p = V.unsafeCast $ imageData img
                  in  (juicyPixelFormat ([]::[p]), V2 w h, p)
