{-# LANGUAGE FlexibleContexts #-}
module Main where

import Codec.FFmpeg
import Codec.FFmpeg.Common
import Codec.FFmpeg.Decode (frameReaderTime)

import Control.Concurrent.MVar (newMVar, takeMVar, putMVar)
import Control.Monad.Except
import Control.Monad.Loops
import Control.Monad.Trans.Maybe

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)
import Data.Text (Text)

import Foreign.C.Types
import Foreign.Ptr

import qualified SDL as SDL


{- Wrapper for frameReaderTime which returns
   time difference between two adjecent frames.   
-}
frameReaderTimeDiff
  :: (MonadIO m, MonadError String m)
  => AVPixelFormat
  -> InputSource
  -> m (IO (Maybe (AVFrame, Double)), IO ())
frameReaderTimeDiff fmt src = do
  
  -- Get reader and cleanup.
  (getFrame, cleanup) <- frameReaderTime fmt src 
  
  -- Put zero time into variable.
  prevTimeVar <- liftIO $ newMVar 0.0
  
  -- Wrapper for reader.
  let getFrame' = runMaybeT $ do
  
        -- Get next frame.
        (frame, currTime) <- frameGet
      
        -- Get time of previous frame.
        prevTime <- prevTimeGet
        
        -- Set time of current frame.
        prevTimeSet currTime
                      
        -- Compute time difference.
        let timeDiff = currTime - prevTime
        
        -- Return frame and difference.
        return (frame, timeDiff)
        
        where
          
          -- Getter for frame.
          frameGet = MaybeT getFrame
          
          -- Getter for previous frame time.
          prevTimeGet = liftIO $ takeMVar prevTimeVar
          
          -- Setter for previous frame time.
          prevTimeSet = liftIO . putMVar prevTimeVar
          
  -- Return wrapper and original cleanup.
  return (getFrame', cleanup)


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
   
   Returned ByteString may be used to fill up SDL's
   textures (using SDL.updateTexture).
   
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
        

-- Image.
data Image =
  Image {
    imgWidth    :: CInt,
    imgHeight   :: CInt,
    imgLineSize :: CInt,
    imgData     :: ByteString
  }


-- AVFrame to Image conversion.
frameToImage :: AVFrame -> IO (Maybe Image)
frameToImage = runMaybeT . frameToImageT 

-- Transformer version of frameToImage.
frameToImageT :: AVFrame -> MaybeT IO Image
frameToImageT frame = MaybeT $ do
  
  w <- getWidth frame
  h <- getHeight frame
  s <- getLineSize frame
  
  fmap (Image w h s) <$> copyImageData frame
         
         
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


  {- Setup frame reader. -}

  -- Initialize FFmpeg.
  liftIO $ initFFmpeg
  
  -- Open input source for reading frames.
  (getImage, cleanup) <- imageReader'
  
  
  {- Render frames. -}
  liftIO $ whileJust_ (nothingOnQuit getImage) $
    \ (image, time) -> do
      
      {- Rendering. -}
      
      -- Rendering start time.
      rStartTime <- SDL.time
      
      -- Create texture from frame.
      texture <- imageToTexture image renderer
      
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
      
      -- Destroy texture.
      SDL.destroyTexture texture
                
      -- Finish time of rendering.
      rFinishTime <- SDL.time
      
      
      {- Synchronizing. -}
      
          -- Total rendering time.
      let rTotalTime = sec2msec $ rFinishTime - rStartTime
          -- Frame time in MS.
          frameTime = sec2msec time

      -- If rendering time is less then frame time.
      when ( time > 0 && rTotalTime < frameTime) $
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
    
    
    imageReader' = do
    
      (getFrame, cleanup) <- frameReaderTimeDiff (cfgFmtFFmpeg cfg) src
      
      let getImage = runMaybeT $ do
            
            (frame, time) <- MaybeT getFrame
            
            image <- frameToImageT frame
            
            return (image, time)
       
      return (getImage, cleanup)
      
      
    imageToTexture (Image w h s d) r = do
      
      -- Create empty texture.
      texture <- SDL.createTexture
                   r
                   (cfgFmtSDL cfg)
                   SDL.TextureAccessStatic
                   $ SDL.V2 w h

      -- Update texture by image from frame.
      texture' <- SDL.updateTexture
                    texture
                    -- Entire texture.
                    Nothing
                    d
                    s
                    
      return texture'
    
     

main :: IO ()
main = return ()
