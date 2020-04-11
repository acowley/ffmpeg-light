module Main where
import Codec.FFmpeg
import Codec.Picture
-- import Codec.Picture.Types (dropTransparency)
import Control.Monad (forM_)
import Graphics.Rasterific
import Graphics.Rasterific.Linear
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Transformations

-- | The Rasterific logo sample shape.
logo :: Int -> Bool -> Vector -> [Primitive]
logo size inv offset = map BezierPrim . bezierFromPath . way $ map (^+^ offset)
    [ (V2   0  is)
    , (V2   0   0)
    , (V2  is   0)
    , (V2 is2   0)
    , (V2 is2  is)
    , (V2 is2 is2)
    , (V2  is is2)
    , (V2  0  is2)
    , (V2  0   is)
    ]
  where is = fromIntegral size
        is2 = is + is

        way | inv = reverse
            | otherwise = id

-- | Sample a quadratic bezier curve.
bezierInterp :: Bezier -> [Point]
bezierInterp (Bezier a b c) = go 0
  where v1 = b - a
        v2 = c - b
        go t
          | t >= 1 = []
          | otherwise = let q0 = a + v1 ^* t
                            q1 = b + v2 ^* t
                            vq = q1 - q0
                        in q0 + vq ^* t : (go $! t + 0.05)

-- | Our animation path.
path :: [Point]
path = concatMap bezierInterp $
       bezierFromPath [ (V2   0  is)
                      , (V2   0   0)
                      , (V2  (is+5)   0)
                      , (V2 (is2+10)   0)
                      , (V2 (is2+10)  is)
                      , (V2 (is2+10) is2)
                      , (V2  (is+5) is2)
                      , (V2  0  is2)
                      , (V2  0   is)
                      ]
  where is = 15
        is2 = is + is

background, blue :: PixelRGBA8
background = PixelRGBA8 128 128 128 255
blue = PixelRGBA8 0 020 150 255

-- `fgSize` will determine our image size. `bgSize` is smaller so we
-- see the effect of the `SamplerRepeat` sampler.

fgSize, fgScale, bgSize :: Float
fgSize = 350
fgScale = fgSize / 100
bgSize = 57 * fgScale

fgSizei :: Integral a => a
fgSizei = floor fgSize

-- | A ring with a drop-shadow on the inside. The texture is repeated,
-- resulting in concentric rings centered at @(200,200)@.
bgGrad :: Texture PixelRGBA8
bgGrad = withSampler SamplerRepeat $
         radialGradientTexture gradDef (V2 bgSize bgSize) (bgSize * 0.5)
  where gradDef = [(0  , PixelRGBA8 255 255 255 255)
                  ,(0.5, PixelRGBA8 255 255 255 255)
                  ,(0.5, PixelRGBA8 255 255 255 255)
                  ,(0.525, PixelRGBA8 255 255 255 255)
                  ,(0.675, PixelRGBA8 128 128 128 255)
                  ,(0.75, PixelRGBA8 100 149 237 255)
                  ,(1, PixelRGBA8 100 149 237 255)
                  ]

-- | Adapted from the Rasterific logo example.
logoTest :: Texture PixelRGBA8 -> Vector -> Image PixelRGBA8
logoTest texture insetOrigin =
  renderDrawing fgSizei fgSizei background (bg >> drawing)
  where
    beziers = logo 40 False $ V2 10 10
    inverse = logo 20 True $ (V2 20 20 + insetOrigin)
    bg = withTexture bgGrad . fill $ rectangle (V2 0 0) fgSize fgSize
    drawing = withTexture texture . fill
            . transform (applyTransformation $ scale fgScale fgScale)
            $ beziers ++ inverse

-- | Animate the logo and write it to a video file!
main :: IO ()
main = do initFFmpeg
          -- Change the output file extension to ".gif" and drop
          -- transparency to get an animated gif! We can get a small
          -- GIF file by setting 'epPixelFormat' to 'avPixFmtRgb8',
          -- but it might not look very good.

          w <- imageWriter params "logo.mov"
          -- w <- (. fmap (pixelMap dropTransparency))
          --      `fmap` imageWriter params "logo.gif"

          forM_ path $ w . Just . logoTest (uniformTexture blue)
          w Nothing
  where params = EncodingParams
                    { epCodec = Nothing
                    , epFormatName = Nothing
                    , epStreamParams = JustVideo (VideoParams fgSizei fgSizei 30 Nothing "")
                    }
        -- tinyGif = params { epPixelFormat = Just avPixFmtRgb8 }
        -- prettyGif = params { epPreset = "dither" }
