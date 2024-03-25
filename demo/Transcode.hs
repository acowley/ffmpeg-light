module Main where

import System.Environment (getArgs)
import qualified Codec.FFmpeg as FF
import qualified Codec.FFmpeg.Encode as FF
import Codec.Picture

type Frame = Image PixelRGB8

usage :: String
usage =
  unlines [ "Usage: transcode inputFile outputFile [outputFormat] [outputWidth] [outputHeight]"
          , "  Example: transcode rtmp://localhost/app/one rtmp://localhost/app/two flv 640 480"
          , ""
          , "  Copies the content from inputFile to outputFile using H264."
          , "  Defaults:"
          , "  outputFormat=flv"
          , "  outputWidth=640"
          , "  outputHeight=480"
          ]

main :: IO ()
main = do
  args <- getArgs
  FF.initFFmpeg
  FF.setLogLevel FF.avLogDebug
  case args of
    [from, to] -> copy from to "flv" 640 480
    [from, to, format, w, h] -> copy from to format (read w) (read h)
    _ -> error usage

copy :: FilePath -> FilePath -> String -> Int -> Int -> IO ()
copy from to format w h = do
  let ep = (FF.defaultH264 (fromIntegral w) (fromIntegral h))
            -- { FF.epFormatName = Just format }
            -- TODO: get this working again
  (getFrame, cleanup, _) <- FF.imageReader (FF.File from)
  putFrame <- FF.imageWriter ep to
  loop getFrame cleanup putFrame (\x -> return x)

loop :: IO (Maybe Frame)
     -> IO cleanup
     -> (Maybe Frame -> IO ())
     -> (Frame -> IO Frame)
     -> IO cleanup
loop getFrame finishReading putFrame editFrame = do
  maybeFrame <- getFrame
  case maybeFrame of
    Nothing -> do
      putFrame Nothing
      finishReading
    Just x -> do
      x' <- editFrame x
      putFrame (Just x')
      loop getFrame finishReading putFrame editFrame
