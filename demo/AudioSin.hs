module Main where

import           Codec.FFmpeg
import           Codec.FFmpeg.AudioStream
import           Codec.FFmpeg.Common
import           Codec.FFmpeg.Decode
import           Codec.FFmpeg.Encode
import           Codec.FFmpeg.Enums
import           Codec.FFmpeg.Resampler
import           Codec.FFmpeg.Types
import           Control.Monad.Except
import           Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Storable     as VS
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import Numeric
import           System.Environment

-- Simple Music DSL
type Sound = Float -> Float

volume :: Float -> Sound -> Sound
volume v s t = v * (s t)

mkNote :: Float -> Sound
mkNote f = \t -> sin (f * 2 * pi * t)

a4 :: Sound
a4 = mkNote 440

c4 :: Sound
c4 = mkNote 261.6256

d4 :: Sound
d4 = mkNote 293.6648

e4 :: Sound
e4 = mkNote 329.6276

f4 :: Sound
f4 = mkNote 349.2282

g4 :: Sound
g4 = mkNote 391.9954

b4 :: Sound
b4 = mkNote 493.8833

c5 :: Sound
c5 = mkNote 523.2511

e5 :: Sound
e5 = mkNote 659.2551

combineSounds :: [Sound] -> Sound
combineSounds snds t = sum $ map (\snd -> snd t) snds

-- Take sounds and their duration and put them in order
sequenceSounds :: [(Float, Sound)] -> Sound
sequenceSounds snds tin = go tin snds
  where
    go t [(_,snd)] = snd t
    go t ((dur,snd):rest)
      | t <= dur   = snd t
      | otherwise = go (t-dur) rest
    go _ _ = 0

cMaj7 = combineSounds [ volume 0.23 c4, volume 0.23 e4, volume 0.23 g4, volume 0.23 b4 ]
dMin7 = combineSounds [ volume 0.23 d4, volume 0.23 f4, volume 0.23 a4, volume 0.23 c5 ]
g7    = combineSounds [ volume 0.23 g4, volume 0.23 a4, volume 0.23 c5, volume 0.23 e5 ]
-- ii - V7 - I jazz chord progression
twoFiveOne = sequenceSounds [(1, dMin7), (1, g7), (1, cMaj7)]

main :: IO ()
main = do
    args <- getArgs
    case args of
      [fname] -> do
        initFFmpeg
        let outOpts = AudioOpts
                        { aoChannelLayout = avChLayoutMono
                        , aoSampleRate = 44100
                        , aoSampleFormat = avSampleFmtS16p
                        }
            encParams = JustAudio outOpts
        (_, (ctx, audioWriter)) <- frameWriter encParams fname
        frame <- frame_alloc_check
        setNumSamples frame =<< getFrameSize ctx
        setFormat frame . getSampleFormatInt =<< getSampleFormat ctx
        setChannelLayout frame =<< getChannelLayout ctx

        ret <- av_frame_get_buffer frame 0
        when (ret < 0) (error "alloc buffers")

        let sampleRate = aoSampleRate outOpts

        forM_ [0..120] $ \i -> do
          av_frame_make_writable frame
          dataPtr <- castPtr <$> getData frame :: IO (Ptr CShort)
          nbSamples <- getNumSamples frame
          forM_ [0..nbSamples-1] $ \j -> do
            let idx = fromIntegral i * fromIntegral nbSamples + fromIntegral j :: Integer
                t = fromIntegral idx / fromIntegral sampleRate
                v = twoFiveOne t
                v16 = round (v * fromIntegral (maxBound :: CShort))
            poke (advancePtr dataPtr (fromIntegral j)) v16
          audioWriter (Just frame)

        -- Write nothing to close final and finalize
        audioWriter Nothing

      _ -> error $ concat usage
  where
    usage = [ "Supply an out final name to write sinusoidal music to a file" ]
