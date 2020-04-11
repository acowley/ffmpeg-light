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
              case eRes of
                Left er -> error er
                Right (as, getFrame, cleanup) -> do
                  putStrLn $ "bitrate : " ++ show (asBitRate as)
                  putStrLn $ "sample rate : " ++ show (asSampleRate as)
                  putStrLn $ "sample format : " ++
                      show (getSampleFormatInt (asSampleFormat as))
                  putStrLn $ "channel layout : " ++ show (asChannelLayout as)
                  putStrLn $ "channel count : " ++ show (asChannelCount as)
                  let inParams = AudioParams
                                  { apChannelLayout = asChannelLayout as
                                  , apSampleRate = asSampleRate as
                                  , apSampleFormat = asSampleFormat as
                                  }
                      outParams = AudioParams
                                  { apChannelLayout = asChannelLayout as
                                  , apSampleRate = 44100
                                  , apSampleFormat = asSampleFormat as
                                  }
                      encParams = EncodingParams
                                    { epCodec = Nothing
                                    , epFormatName = Nothing
                                    , epStreamParams = JustAudio outParams
                                    }
                  (_, mCtx, _, audioWriter) <- frameWriter encParams outname
                  case mCtx of
                    Nothing -> error "Didn't get audio context"
                    Just ctx -> do
                      (sendFrame, getResampledFrame) <- makeResampler ctx inParams outParams
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
                                fs <- getNumSamples frame
                                audioWriter (Just frame)
                                readAndWrite
                      go 1
                      audioWriter Nothing

                      cleanup
                      return ()
              return ()
            _ -> error $ concat usage
  where
    usage = [ "Supply an input video to extract the audio file to a wav file" ]
