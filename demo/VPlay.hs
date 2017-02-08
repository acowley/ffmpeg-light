{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.FFmpeg
import Codec.FFmpeg.Common
import Codec.FFmpeg.Decode hiding (av_malloc)

import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.Trans.Maybe

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)
import Data.Maybe (isJust, fromJust)
import Data.Text (Text)

import Foreign.C.Types
import Foreign.Ptr

import System.Environment

import qualified SDL as SDL


{- Video player example. -}


{- Auxiliary functions. -}


{- Wrapper for monads which returns Nothing when:

     1. Monad action returns Nothing. 
     2. When condition holds.
-}
nothingWhen
  :: Monad m
  => m a -> (a -> Bool) -> m (Maybe b) -> m (Maybe b)
nothingWhen g p action = do
  
  -- Generate conditional.
  a <- g
  
  -- Check predicate.
  if p a
    then return Nothing
    else action
      

{- Wrapper for IO action which returns Nothing when:

    1. Action by itself returns Nothing.
    2. If QuitEvent was received.

-}
nothingOnQuit
  :: MonadIO m
  => m (Maybe a) -> m (Maybe a)
nothingOnQuit action =
  nothingWhen
    SDL.pollEvents
    (not . null . filter
      (\ event ->
          case SDL.eventPayload event of 
            SDL.QuitEvent -> True
            _             -> False)) action
            
         
{- Returns a byte-string containing an image data.
   
   Returned ByteString doesn't refer back to it's
   source AVFrame. So, source frame may be deleted
   or changed, but image will stay.  
   
   I'm not sure about using unsafePerformIO here.
-}
copyImageData :: AVFrame -> IO (Maybe ByteString)
copyImageData frame =
  runMaybeT $ do
    
    -- Get required size of buffer to hold image data.
    imageBufSize <- frameBufferSizeT frame
                         
    -- Allocate buffer to hold image data.
    imageBuf <- MaybeT $
       Just <$> (av_malloc $ fromIntegral imageBufSize)
    
    -- Image data buffer cleanup.
    let imageBufCleanup = av_free imageBuf
    
    -- Copy image to buffer.
    _ <- frameCopyToBufferT frame (castPtr imageBuf)
    
    -- Fill up byte-string by data from buffer.
    MaybeT $ Just <$>
      unsafePackCStringFinalizer
        (castPtr imageBuf)
        (fromIntegral imageBufSize)
        -- Cleanup for buffer.
        imageBufCleanup

-- Transformer version of copyImage.        
copyImageDataT :: AVFrame -> MaybeT IO ByteString
copyImageDataT = MaybeT . copyImageData
        
         
-- Configuration for video player.
data Config =
  Config {
    cfgDriver     :: CInt,
    cfgFmtFFmpeg  :: AVPixelFormat,
    cfgFmtSDL     :: SDL.PixelFormat,
    cfgWindowName :: Text
  }
  
  
-- Converts floating point seconds to milliseconds.
sec2msec :: (RealFrac a, Integral b) => a -> b
sec2msec = floor . (*1000)
            

{- Main function. -}

videoPlayer
  :: (MonadIO m, MonadError String m)
  => Config -> InputSource -> m ()
videoPlayer cfg src = do
  
  {- Setup SDL. -}

  -- Initialize SDL.
  SDL.initializeAll
  
  -- Create window.
  window <- createWindow
  
  -- Get renderer associated with window.
  renderer <- createRenderer window


  {- Setup texture reader. -}

  -- Initialize FFmpeg.
  liftIO $ initFFmpeg
  
  -- Open input source for reading textures.
  (getTexture, cleanup) <- storeTimeDiff $ textureReaderTime renderer
  
  
  {- Render frames. -}
  liftIO $ whileJust_ (nothingOnQuit getTexture) $
    \ (texture, time) -> do
      
      {- Rendering. -}
      
      -- Rendering start time.
      rStartTime <- SDL.time
      
      -- Copy texture to renderer.
      SDL.copy
        renderer
        texture
        -- Entire texture.
        Nothing
        -- Entire rendering target.
        Nothing
      
      -- Present renderer using present.
      SDL.present renderer
                
      -- Finish time of rendering.
      rFinishTime <- SDL.time
      
      
      {- Synchronizing. -}
      
          -- Total rendering time.
      let rTotalTime = sec2msec $ rFinishTime - rStartTime
          -- Frame time in MS.
          frameTime = sec2msec time

      -- If rendering time is less then frame time.
      when ( time > 0 && rTotalTime < frameTime) $ do
        -- Sleep their difference.
        SDL.delay $ frameTime - rTotalTime
      
      
  {- Cleanup. -}
    
  -- Cleanup after frame reading.
  liftIO cleanup
  
  -- Destroy renderer.
  SDL.destroyRenderer renderer
  
  -- Destroy window.
  SDL.destroyWindow window
  
  -- Quit SDL.
  SDL.quit 
  
  where
  
    createWindow     = SDL.createWindow (cfgWindowName cfg) SDL.defaultWindow
    createRenderer w = SDL.createRenderer w (cfgDriver cfg) SDL.defaultRenderer
    
    createTexture r w =
      SDL.createTexture r (cfgFmtSDL cfg) SDL.TextureAccessStreaming . SDL.V2 w
       
       
    {- Modified frameReaderTime: it produces textures. -}
    textureReaderTime renderer =
      do let dstFmt = cfgFmtFFmpeg cfg
         
         inputContext <- openInput src
         checkStreams inputContext
         (vidStreamIndex, ctx, cod, vidStream) <- findVideoStream inputContext
         
         -- Data needed for texture creation.
         w <- liftIO $ getWidth ctx
         h <- liftIO $ getHeight ctx
         let std = avPixelStride dstFmt
         when (not $ isJust std) $
           throwError $ "Unsupported format: " ++ show dstFmt
         let s = w * (fromIntegral . fromJust) std
         
         -- Texture which will be updated.
         texture <- createTexture renderer w h
         
         _ <- openCodec ctx cod
         (reader, cleanup) <- prepareReader inputContext vidStreamIndex dstFmt ctx
         AVRational num den <- liftIO $ getTimeBase vidStream
         let (numl, dend) = (fromIntegral num, fromIntegral den)
             frameTime' frame =
               do n <- getPts frame
                  return $ fromIntegral (n * numl) / dend
             readTS = do frame <- reader
                         case frame of
                           Nothing -> return Nothing
                           Just f -> do t <- frameTime' f
                                        return $ Just (f, t)
                       
             -- Texture reader.                               
         let readTexture = runMaybeT $ do
                             
                             (f, t)   <- MaybeT readTS
                             img      <- copyImageDataT f
                             texture' <- liftIO $ updateTexture texture img
                             
                             return (texture', t)
                             
                             where
                               updateTexture t img =
                                 -- Update entire texture.
                                 SDL.updateTexture t Nothing img s
             
             -- Texture cleanup.                
             cleanup' = cleanup >> SDL.destroyTexture texture
         
         return (readTexture, cleanup')
         
         
    {- Modify reader to return time difference
       between current and previous frames.      -}
    storeTimeDiff reader = do
      
      -- Get producer and cleanup.
      (getNext, cleanup) <- reader
      
      -- Put zero time into variable.
      prevTimeVar <- liftIO $ newMVar 0.0
      
      -- Wrapper for reader.
      let getNext' = runMaybeT $ do
      
            -- Get next.
            (next, currTime) <- MaybeT getNext
          
            -- Get time of previous.
            prevTime <- prevTimeGet
            
            -- Set time of current.
            prevTimeSet currTime
                          
            -- Compute time difference.
            let timeDiff = currTime - prevTime
            
            -- Return next and difference.
            return (next, timeDiff)
            
            where
              
              -- Getter for previous time.
              prevTimeGet = liftIO $ takeMVar prevTimeVar
              
              -- Setter for previous time.
              prevTimeSet = liftIO . putMVar prevTimeVar
              
      -- Return wrapper.
      return (getNext', cleanup)


{- Main. -}

-- Default configuration.
defaultConfig :: Config
defaultConfig =
  Config {
    cfgDriver     = (-1),
    cfgFmtFFmpeg  = avPixFmtRgb24,
    cfgFmtSDL     = SDL.RGB24,
    cfgWindowName = "VPLay"
  }
  
-- Runs videoPlayer in Either monad.
runVideoPlayer :: Config -> FilePath -> IO (Either String ())
runVideoPlayer cfg = runExceptT . videoPlayer cfg . File

-- Video player with default configuration.
-- Command line argument: path to video file.
main :: IO ()
main = getArgs >>= runVideoPlayer defaultConfig . head >> return ()
