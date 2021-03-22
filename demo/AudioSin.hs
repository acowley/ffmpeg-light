{-# LANGUAGE MultiWayIf #-}
module Main where

import           Codec.FFmpeg
import           Codec.FFmpeg.AudioStream
import           Codec.FFmpeg.Common
import           Codec.FFmpeg.Encode
import           Codec.FFmpeg.Enums
import           Codec.FFmpeg.Juicy
import           Codec.FFmpeg.Types
import           Codec.Picture
import           Control.Monad.Except
import           Data.IORef
import           Foreign.C.Types
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
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
combineSounds snds t = sum $ map (\s -> s t) snds

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

mkImage :: Int -> Int -> PixelRGB8 -> Image PixelRGB8
mkImage w h color =
  generateImage (\_ _ -> color) w h

main :: IO ()
main = do
  initFFmpeg

  let w = 1080
      h = 720
      encParams = AVEncodingParams
                    { avepWidth = w
                    , avepHeight = h
                    , avepFps = 30
                    , avepCodec = Nothing
                    , avepPixelFormat = Nothing
                    , avepChannelLayout = avChLayoutMono
                    , avepSampleRate = 44100
                    , avepSampleFormat = avSampleFmtFltp
                    , avepPreset = ""
                    , avepFormatName = Nothing
                    }
  writerContext <- audioVideoWriter encParams "sinusoidal.mp4"
  let mCtx = avwAudioCodecContext writerContext
      videoWriter = avwVideoWriter writerContext
      audioWriter = avwAudioWriter writerContext
  case mCtx of
    Nothing -> error "Could not get audio ctx"
    Just ctx -> do
      frame <- frame_alloc_check
      setNumSamples frame =<< getFrameSize ctx
      setFormat frame . getSampleFormatInt =<< getSampleFormat ctx
      setChannelLayout frame =<< getChannelLayout ctx
      setSampleRate frame =<< getSampleRate ctx

      ch <- getChannelLayout ctx
      numChannels <- getChannels ctx

      print ("Channel Layout", ch)
      print ("Channels", numChannels)

      runWithError "Alloc buffers" (av_frame_get_buffer frame 0)

      let sampleRate = avepSampleRate encParams
      print ("sample rate", sampleRate)

      vidFrameRef <- newIORef 0 :: IO (IORef Int)
      forM_ [0..120] $ \i -> do
        av_frame_make_writable frame
        dataPtr <- castPtr <$> getData frame :: IO (Ptr CFloat)
        nbSamples <- getNumSamples frame
        forM_ [0..nbSamples-1] $ \j -> do
          let idx = fromIntegral i * fromIntegral nbSamples + fromIntegral j :: Integer
              t = fromIntegral idx / fromIntegral sampleRate
              v = twoFiveOne t
          poke (advancePtr dataPtr (fromIntegral j)) (realToFrac v)
          vidFrame <- readIORef vidFrameRef
          when (t * 30 >= fromIntegral vidFrame) $ do
            -- TODO: I'm not sure why t seems to be half the actual value but I need to do
            -- 0.5 and 1 to make the chord changes match up with the color changes
            modifyIORef vidFrameRef (+1)
            let color = if | t <= 1    -> PixelRGB8 255 0 0
                           | t <= 2    -> PixelRGB8 0 255 0
                           | otherwise -> PixelRGB8 0 0 255
                img = mkImage (fromIntegral w) (fromIntegral h) color
            videoWriter (Just (fromJuciy img))
        audioWriter (Just frame)

      videoWriter Nothing
      audioWriter Nothing
