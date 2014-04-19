import Codec.FFmpeg
import Codec.Picture
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Trans.Maybe
import qualified Data.Vector.Storable as V
import System.Environment

-- The example used in the README
firstFrame :: IO (Maybe DynamicImage)
firstFrame = do (getFrame, cleanup) <- frameReaderT "myVideo.mov"
                (runMaybeT $ getFrame >>= toJuicyT) <* cleanup

-- | Generate a video that pulses from light to dark.
pulseVid :: IO ()
pulseVid =
  do boom <- frameWriter (defaultParams sz sz) "pulse.mov"
     let go :: Int -> Int -> Int -> IO ()
         go 600 _ _ = boom Nothing
         go n d i = do boom (Just $ V.replicate (fromIntegral $ sz*sz*3) 
                                                (fromIntegral i))
                       let i' = i + d
                       if i' < 100 
                       then go (n+1) 1 101
                       else if i' > 255 
                            then go (n+1) (-1) 254
                            else go (n+1) d i'
                
     go 0 (-1) 255
  where sz = 64

testEncode :: IO ()
testEncode = initFFmpeg >> pulseVid >> putStrLn "All done!"

testDecode :: FilePath -> IO ()
testDecode vidFile = 
  do initFFmpeg
     (getFrame, cleanup) <- frameReaderTime vidFile
     frame1 <- getFrame
     case frame1 of
       Just (avf,ts) -> do putStrLn $ "Frame at "++show ts
                           saveJuicy "frame1.png" avf
       Nothing -> putStrLn "No frame for me :("
     replicateM_ 299 getFrame
     frame2 <- getFrame
     case frame2 of
       Just (avf,ts) -> do putStrLn $ "Frame at "++show ts
                           saveJuicy "frame2.png" avf
       Nothing -> putStrLn "No frame for me :("
     cleanup
     putStrLn "All done!"

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> testEncode
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

