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
import Data.Text (Text)

import Data.IORef

import Foreign.C.Types
import Foreign.Ptr

import System.Environment

import qualified SDL as SDL


{- Auxiliary. -}

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
    updateTexture t img =
      frameLineSizeT frame >>=
        SDL.updateTexture t Nothing img

-- Update texture by image data from frame.
updateTextureByFrame :: SDL.Texture -> AVFrame -> IO (Maybe SDL.Texture)
updateTextureByFrame t = runMaybeT . updateTextureByFrameT t

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
            SDL.KeyboardEvent ev ->
              SDL.keysymKeycode (SDL.keyboardEventKeysym ev) == SDL.KeycodeEscape
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

-- Convert floating point second to millisecond.
sec2msec :: (RealFrac a, Integral b) => a -> b
sec2msec = floor . (*1000)

-- Adjust window size by display size.
-- It uses first display retrieved from call to SDL.getDisplays.
-- I don't know yet how to get a display where window is opened.
-- So this function won't be used right now.
adjustWindowSize :: MonadIO m => SDL.Window -> m ()
adjustWindowSize w = do
  (SDL.V2 ww wh) <- SDL.get (SDL.windowSize w)
  (SDL.V2 dw dh) <- SDL.displayBoundsSize <$> firstDisplay
  let w' = min ww dw
      h' = min wh dh
  (SDL.windowSize w) SDL.$= (SDL.V2 w' h')
  where
    firstDisplay = SDL.getDisplays >>= return . head


{- Main. -}

-- Configuration for video player.
data Config =
  Config
    { cfgWindowTitle    :: Text
    , cfgRendererDriver :: CInt
    , cfgFmtFFmpeg      :: AVPixelFormat
    , cfgFmtSDL         :: SDL.PixelFormat
    }

videoPlayer
  :: (MonadIO m, MonadError String m)
  => Config -> InputSource -> m ()
videoPlayer cfg src = do

  {- Setup. -}

  liftIO initFFmpeg
  SDL.initializeAll

  (renderTexture, getTexture, cleanup) <- textureReader src

  -- First frame begins.
  timeRef <- liftIO . newIORef =<< SDL.time

  liftIO $ whileJust_ (nothingOnQuit getTexture) $
    \ (next, time) -> do

      {- Rendering. -}

      -- Rendering start time.
      rStartTime <- liftIO $ readIORef timeRef

      renderTexture next

      -- Finish time of rendering.
      rFinishTime <- SDL.time :: IO Double

      {- Synchronizing. -}

          -- Total rendering time.
      let rTotalTime = sec2msec $ rFinishTime - rStartTime
          -- Frame time in MS.
          frameTime = sec2msec time

      -- If rendering time is less then frame time.
      when ( time > 0 && rTotalTime < frameTime) $ do
        -- Sleep their difference.
        SDL.delay $ frameTime - rTotalTime

      -- Next frame begins.
      liftIO . writeIORef timeRef =<< SDL.time

  {- Cleanup. -}

  liftIO cleanup
  SDL.quit

  where

    -- Create window using title from config.
    createWindow w h = do
      window <- SDL.createWindow (cfgWindowTitle cfg) SDL.defaultWindow
      (SDL.$=) (SDL.windowSize window) (SDL.V2 w h)
      return window

    -- Create renderer using driver from config.
    createRenderer window =
      SDL.createRenderer window (cfgRendererDriver cfg) SDL.defaultRenderer

    -- Create texture using pixel format from config.
    createTexture renderer w h =
      SDL.createTexture
        renderer
        (cfgFmtSDL cfg)
        SDL.TextureAccessStreaming
        (SDL.V2 w h)

    -- Return texture reader, renderer and cleanup.
    textureReader :: (MonadIO m', MonadError String m', MonadIO m)
                  => InputSource
                  -> m' ( SDL.Texture -> m ()
                        , IO (Maybe (SDL.Texture, Double))
                        , IO ())
    textureReader inp = do

      -- Open video.
      inputContext <- prepareInput inp
      (vsIdx, ctx, _, vs, _) <- prepareVideoCodec inputContext

      -- Get frame size.
      textureWidth <- liftIO $ getWidth ctx
      textureHeight <- liftIO $ getHeight ctx

      -- Compute window size. If the pixels aren't square, stretch the window,
      -- SDL will automatically scale the texture to fit.
      par <- liftIO $ guessAspectRatio ctx

      let pixelAspectRatio :: Double
          pixelAspectRatio = fromIntegral (numerator par) / fromIntegral (denomenator par)

          windowWidth, windowHeight :: CInt
          windowWidth = round (pixelAspectRatio * fromIntegral textureWidth)
          windowHeight = textureHeight

      -- Create window, renderer and texture.
      window   <- createWindow windowWidth windowHeight
      renderer <- createRenderer window
      texture  <- createTexture renderer textureWidth textureHeight

      -- Create frame reader.
      let dstFmt = cfgFmtFFmpeg cfg
      (reader, cleanup) <- prepareReader inputContext vsIdx dstFmt ctx

      -- Transform reader to read frame time.
      timeBase     <- liftIO $ getTimeBase vs
      tsDiffReader <- readTSDiff (readTS timeBase reader)

          -- Texture reader.
      let reader' = runMaybeT $ do
                          (f, t) <- MaybeT tsDiffReader
                          updateTextureByFrameT texture f
                            >>= return . flip (,) t

          -- Texture renderer.
          render t = do
            SDL.copy renderer t Nothing Nothing
            SDL.present renderer

          -- New cleanup.
          cleanup' = cleanup
                       >> SDL.destroyTexture  texture
                       >> SDL.destroyRenderer renderer
                       >> SDL.destroyWindow   window

      return (render, reader', cleanup')


{- Main. -}

-- Default configuration.
defaultConfig :: Config
defaultConfig =
  Config
    { cfgWindowTitle    = "VPLay"
    , cfgRendererDriver = (-1) -- find driver automatically.
    , cfgFmtFFmpeg      = avPixFmtRgb24
    , cfgFmtSDL         = SDL.RGB24
    }

-- Runs videoPlayer in Either monad.
runVideoPlayer :: Config -> FilePath -> IO (Either String ())
runVideoPlayer cfg = runExceptT . videoPlayer cfg . File

-- Video player with default configuration.
-- Command line argument: path to video file.
main :: IO ()
main = getArgs >>= runVideoPlayer defaultConfig . head >> return ()
