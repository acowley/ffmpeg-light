module Main where

import           Codec.FFmpeg
import           Codec.FFmpeg.AudioStream
import           Codec.FFmpeg.Decode
import           Codec.FFmpeg.Encode
import           Codec.FFmpeg.Resampler
import           Control.Monad.Except
import           System.Environment


main :: IO ()
main = do initFFmpeg
          args <- getArgs

          case args of
            [fname, outname] -> do
              eRes <- runExceptT $ frameAudioReader (File fname)
              case eRes of
                Left er -> error er
                Right (as, getFrame, cleanup, _) -> do
                  putStrLn $ "bitrate : " ++ show (asBitRate as)
                  putStrLn $ "sample rate : " ++ show (asSampleRate as)
                  putStrLn $ "sample format : " ++
                      show (getSampleFormatInt (asSampleFormat as))
                  let chLayout = asChannelLayout as
                  putStrLn $ "channel layout order: " ++ show (order chLayout)
                  putStrLn $ "channel layout channels: " ++ show (numChannels chLayout)
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
                      encParams = AEncodingParams
                                  { aepChannelLayout = apChannelLayout outParams
                                  , aepSampleRate = apSampleRate outParams
                                  , aepSampleFormat = apSampleFormat outParams
                                  , aepPreset = ""
                                  , aepFormatName = Nothing
                                  }
                  (mCtx, audWriter) <- audioWriter encParams outname
                  case mCtx of
                    Nothing -> error "Didn't get audio context"
                    Just ctx -> do
                      (sendFrame, getResampledFrame) <- makeResampler ctx inParams outParams
                      let go :: Int -> IO ()
                          go i = do
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
                                audWriter (Just frame)
                                readAndWrite
                      go 1
                      audWriter Nothing

                      cleanup
                      return ()
              return ()
            _ -> putStrLn usage
  where
    usage = "Supply an input video and output filename to extract the audio file to a mp3 file"
