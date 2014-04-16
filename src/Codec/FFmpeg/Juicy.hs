-- | Convert between FFmpeg frames and JuicyPixels images.
module Codec.FFmpeg.Juicy where
import Codec.Picture
import Codec.FFmpeg.Enums
import Codec.FFmpeg.Types
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Monad (when, (>=>))
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign.C.Types
import Foreign.Marshal.Array (advancePtr, copyArray)
import Foreign.Ptr (castPtr, Ptr)

-- | Convert an RGB24 'AVFrame' to a 'DynamicImage'.
juicyRGB :: AVFrame -> IO DynamicImage
juicyRGB frame = do
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
  return $ ImageRGB8 (Image w h (V.unsafeCast v'))

-- | Save an RGB24 'AVFrame' to a PNG file on disk.
saveJuicyRGB :: FilePath -> AVFrame -> IO ()
saveJuicyRGB name = juicyRGB >=> savePngImage name 
