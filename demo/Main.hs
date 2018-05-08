module Main where
import Codec.FFmpeg
import Codec.Picture
import Control.Monad (replicateM_)
import qualified Data.Time.Clock as C
import qualified Data.Vector.Storable as V
import System.Environment
import Control.Monad (unless)

-- The example used in the README
firstFrame :: IO (Maybe DynamicImage)
firstFrame = do initFFmpeg
                (getFrame, cleanup) <- imageReader (File "myVideo.mov")
                (fmap ImageRGB8 <$> getFrame) <* cleanup

-- | Generate a video that pulses from light to dark.
pulseVid :: IO ()
pulseVid =
  do boom <- imageWriter (defaultParams sz sz) "pulse.mov"
     let boom' = (boom :: Maybe (Image Pixel8) -> IO ())  . Just . Image sz sz
         go :: Int -> Int -> Int -> IO ()
         go 600 _ _ = boom Nothing
         go n d i = do boom' $ V.replicate (sz*sz) (fromIntegral i)
                       let i' = i + d
                       if i' < 100
                       then go (n+1) 1 101
                       else if i' > 255
                            then go (n+1) (-1) 254
                            else go (n+1) d i'
     go 0 (-1) 255
  where sz :: Integral a => a
        sz = 64

-- | Generate a video that fades from white to gray to white.
testEncode :: IO ()
testEncode = initFFmpeg >> pulseVid >> putStrLn "All done!"

-- | Decoding example. Try changing 'ImageRGB8' to 'ImageY8' in the
-- 'savePngImage' lines to automatically decode to grayscale images!
testDecode :: FilePath -> IO ()
testDecode vidFile =
  do initFFmpeg
     (getFrame, cleanup) <- imageReaderTime (File vidFile)
     frame1 <- getFrame
     case frame1 of
       Just (avf,ts) -> do putStrLn $ "Frame at "++show ts
                           savePngImage "frame1.png" (ImageRGB8 avf)
       Nothing -> putStrLn "No frame for me :("
     replicateM_ 299 getFrame
     frame2 <- getFrame
     case frame2 of
       Just (avf,ts) -> do putStrLn $ "Frame at "++show ts
                           savePngImage "frame2.png" (ImageRGB8 avf)
       Nothing -> putStrLn "No frame for me :("
     cleanup
     putStrLn "All done!"

-- | @loopFor timeSpan action@ repeats @action@ until at least @timeSpan@
-- seconds have elapsed.
loopFor :: Double -> IO () -> IO ()
loopFor time m =
  do start <- C.getCurrentTime
     let go = do m
                 now <- C.getCurrentTime
                 unless (realToFrac (C.diffUTCTime now start) >= time) go
     go

testCamera :: IO ()
testCamera =
  do initFFmpeg -- Defaults to quiet (minimal) logging
     -- setLogLevel avLogInfo -- Restore standard ffmpeg logging
     (getFrame, cleanup) <- imageReader (Camera "0:0" defaultCameraConfig)
     frame1 <- getFrame
     case frame1 of
       img@(Just (Image w h _)) ->
         do let [w',h'] = map fromIntegral [w,h]
            writeFrame <- imageWriter (defaultParams w' h') "camera.mov"
            writeFrame (img :: Maybe (Image PixelRGB8))
            let go = getFrame >>= writeFrame
            loopFor 10 go
            writeFrame Nothing
       _ -> putStrLn "Couldn't read the first frame from the camera"
     cleanup

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> testEncode
            [s]
              | s `elem` ["--help", "-help", "-h"] -> error usage
              | s == "cam" -> testCamera
            [vidFile] -> testDecode vidFile
            _ -> error usage
  where usage =
          unlines [ "Usage: demo [videoFile]"
                  , "  If no argument is given, a test video named "
                  , "  pulse.mov is generated."
                  , ""
                  , "  If a file name is given, then two frames are "
                  , "  extracted: the first frame, and the 301st."
                  , "  These are saved to frame1.png and frame2.png" ]
