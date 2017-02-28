{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Codec.FFmpeg
import Codec.FFmpeg.Common
import Codec.FFmpeg.Decode hiding (av_malloc)

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
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

import Control.Arrow

import qualified SDL as SDL


{- Video player example. -}


{- Auxiliary functions. -}


-- Prepare input source: open, check for streams.
prepareInput :: (MonadIO m, MonadError String m)
             => InputSource
             -> m AVFormatContext
prepareInput inp =
  openInput inp >>= \ ctx -> checkStreams ctx >> return ctx

-- Prepare video codec: find video stream, open.
prepareVideoCodec :: (MonadIO m, MonadError String m)
                  => AVFormatContext
                  -> m ( CInt
                       , AVCodecContext
                       , AVCodec
                       , AVStream
                       , AVDictionary )
prepareVideoCodec inpCtx = do
  (vidStreamIndex, ctx, cod, vidStream) <- findVideoStream inpCtx
  dict <- openCodec ctx cod
  return (vidStreamIndex, ctx, cod, vidStream, dict)


-- Transform reader to return timestamp too. 
readTS :: (HasPts f, Fractional t)
       => AVRational
       -> IO (Maybe f)
       -> IO (Maybe (f, t))
readTS (AVRational num den) reader =
  -- It's part of frameReaderTime definition.
  let (numl, dend) = (fromIntegral num, fromIntegral den)
      frameTime' frame =
        do n <- getPts frame
           return $ fromIntegral (n * numl) / dend
      reader' = do frame <- reader
                   case frame of
                     Nothing -> return Nothing
                     Just f -> do t <- frameTime' f
                                  return $ Just (f, t) in reader'
                                  

-- Transform frame and timestamp reader to compute frame
-- time as a difference between adjacent timestamps. 
readTSDiff :: (MonadIO m, MonadIO m', Num t)
           => m' (Maybe (f, t)) -> m (m' (Maybe (f, t)))
readTSDiff readerTS = do
  timeVar <- liftIO $ newMVar 0
  let reader = runMaybeT $ do
                 (f, currTime) <- MaybeT readerTS
                 prevTime <- takeTime
                 putTime currTime
                 let timeDiff = currTime - prevTime
                 return (f, timeDiff)
                 where
                   takeTime = liftIO . takeMVar $ timeVar
                   putTime  = liftIO . putMVar timeVar
  return reader
  

-- Transformer version of updateTextureByFrame.
updateTextureByFrameT :: SDL.Texture -> AVFrame -> MaybeT IO SDL.Texture
updateTextureByFrameT texture frame =
  copyImageDataT frame >>= updateTexture texture
  where
    updateTexture texture img =
      frameLineSizeT frame >>=
        SDL.updateTexture texture Nothing img 
    
-- Update texture by image data from frame.
updateTextureByFrame :: SDL.Texture -> AVFrame -> IO (Maybe SDL.Texture)
updateTextureByFrame t = runMaybeT . updateTextureByFrameT t


-- Transform frame and time readers frames into textures.
readToTexture :: Num t
              => SDL.Texture
              -> IO (Maybe (AVFrame, t)) 
              -> IO (Maybe (SDL.Texture, t))
readToTexture texture reader =
  runMaybeT $ do
    (f, t) <- MaybeT reader
    updateTextureByFrameT texture f
      >>= return . flip (,) t

-- Transform reader to convert frames to textures and return frame time.
readToTextureWithTSDiff :: (MonadIO m, Fractional t)
                        => AVRational
                        -> SDL.Texture
                        -> IO (Maybe AVFrame)
                        -> m (IO (Maybe (SDL.Texture, t)))
readToTextureWithTSDiff timeBase texture reader =
  readTSDiff (readTS timeBase reader)
    >>= return . readToTexture texture


-- Return Nothing when condition holds.
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
      

-- Retrun Nothing when QuitEvent is received.
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
            
         
{- Return ByteString filled by image data from frame.
   
   Returned ByteString doesn't refer back to it's
   source AVFrame. So, source frame may be deleted
   or changed, but image will stay.  
   
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

-- Transformer version of copyImageData.        
copyImageDataT :: AVFrame -> MaybeT IO ByteString
copyImageDataT = MaybeT . copyImageData
        
         
-- Configuration for video player.
data Config =
  Config
    { cfgWindowTitle    :: Text
    , cfgRendererDriver :: CInt
    , cfgFmtFFmpeg      :: AVPixelFormat
    , cfgFmtSDL         :: SDL.PixelFormat
    }
  
  
-- Convert floating point second to millisecond.
sec2msec :: (RealFrac a, Integral b) => a -> b
sec2msec = floor . (*1000)
            

{- Main function. -}

videoPlayer
  :: (MonadIO m, MonadError String m)
  => Config -> InputSource -> m ()
videoPlayer cfg src = do
  
  
  {- Open video. -}
  
  liftIO $ initFFmpeg
  
  inputContext <- prepareInput src
  (vsIdx, ctx, cod, vs, _) <- prepareVideoCodec inputContext


  {- Create window, renderer and texture. -}
  
  SDL.initializeAll

  w <- liftIO $ getWidth ctx
  h <- liftIO $ getHeight ctx 
  
  window <- createWindow w h
  
  renderer <- createRenderer window
  
  texture <- createTexture renderer w h
               
               
  {- Create texture and time reader. -}
  
  let dstFmt = cfgFmtFFmpeg cfg
  (reader, cleanup) <- prepareReader inputContext vsIdx dstFmt ctx
  
  timeBase <- liftIO $ getTimeBase vs
  
  getTexture <- readToTextureWithTSDiff timeBase texture reader
                
                
  {- Render. -}
  
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

  -- Destroy texture.  
  SDL.destroyTexture texture
  
  -- Destroy renderer.
  SDL.destroyRenderer renderer
  
  -- Destroy window.
  SDL.destroyWindow window
  
  -- Quit SDL.
  SDL.quit
  
  where
    
    createWindow w h = do
      window <- SDL.createWindow (cfgWindowTitle cfg) SDL.defaultWindow
      (SDL.$=) (SDL.windowSize window) (SDL.V2 w h)
      return window
      
    createRenderer window =
      SDL.createRenderer window (cfgRendererDriver cfg) SDL.defaultRenderer

    createTexture renderer w h =
      SDL.createTexture
        renderer
        (cfgFmtSDL cfg)
        SDL.TextureAccessStreaming
        (SDL.V2 w h)
        

{- Main. -}

-- Default configuration.
defaultConfig :: Config
defaultConfig =
  Config
    { cfgWindowTitle    = "VPLay"
    , cfgRendererDriver = (-1)
    , cfgFmtFFmpeg  = avPixFmtRgb24
    , cfgFmtSDL     = SDL.RGB24 
    }
  
-- Runs videoPlayer in Either monad.
runVideoPlayer :: Config -> FilePath -> IO (Either String ())
runVideoPlayer cfg = runExceptT . videoPlayer cfg . File

-- Video player with default configuration.
-- Command line argument: path to video file.
main :: IO ()
main = getArgs >>= runVideoPlayer defaultConfig . head >> return ()
