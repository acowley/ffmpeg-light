module Main where

import           Codec.FFmpeg
import           Codec.FFmpeg.AudioStream
import           Codec.FFmpeg.Decode
import           Codec.FFmpeg.Encode
import           Codec.FFmpeg.Enums
import           Codec.FFmpeg.Resampler
import           Control.Applicative
import           Control.Monad            (replicateM_)
import           Control.Monad.Except
import qualified Data.Vector.Storable     as V
import           System.Environment


main :: IO ()
main = do initFFmpeg
          args <- getArgs

          case args of
            [fname, outname] -> do
              eRes <- runExceptT $ frameAudioReader (File fname)
              putStrLn "here"
              case eRes of
                Left er -> error er
                Right (as, getFrame, cleanup) -> do
                  putStrLn $ "bitrate : " ++ show (asBitRate as)
                  putStrLn $ "sample rate : " ++ show (asSampleRate as)
                  putStrLn $ "sample format : " ++
                      show (getSampleFormatInt (asSampleFormat as))
                  putStrLn $ "channel layout : " ++ show (asChannelLayout as)
                  putStrLn $ "channel count : " ++ show (asChannelCount as)
                  let inOpts = AudioOpts
                                  { aoChannelLayout = asChannelLayout as
                                  , aoSampleRate = asSampleRate as
                                  , aoSampleFormat = asSampleFormat as
                                  }
                      outOpts = AudioOpts
                                  { aoChannelLayout = asChannelLayout as
                                  , aoSampleRate = 44100 -- asSampleRate as
                                  , aoSampleFormat = asSampleFormat as
                                    -- avSampleFmtS16p
                                  }
                      encParams = JustAudio outOpts
                  putStrLn "writer"
                  (_, (ctx, audioWriter)) <- frameWriter encParams outname
                  (sendFrame, getResampledFrame, cleanupResampler) <-
                        makeResampler ctx inOpts outOpts
                  let go i = do
                        mFrame <- getFrame
                        case mFrame of
                          Nothing -> readAndWrite
                          Just frame -> do
                            sendFrame frame
                            readAndWrite
                            go (i+1)
                      readAndWrite = do
                        mFrame <- getResampledFrame
                        case mFrame of
                          Nothing -> return ()
                          Just frame -> do
                            putStrLn "audio writer"
                            fs <- getNumSamples frame
                            putStrLn $ "frame size : " ++ show fs
                            audioWriter (Just frame)
                            putStrLn "after"
                            readAndWrite
                  go 1
                  audioWriter Nothing

                  cleanup
                  cleanupResampler
                  return ()
              return ()
            _ -> error $ concat usage
  where
    usage = [ "Supply an input video to extract the audio file to a wav file" ]
