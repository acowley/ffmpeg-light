module Codec.FFmpeg.Resampler where

import           Codec.FFmpeg.Common
import           Codec.FFmpeg.Enums
import           Codec.FFmpeg.Types
import           Control.Concurrent.STM.TChan
import           Control.Monad                (void, when)
import           Control.Monad.STM
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

foreign import ccall "swr_get_delay"
  swr_get_delay :: SwrContext -> CLong -> IO CLong

foreign import ccall "swr_convert"
  swr_convert :: SwrContext -> Ptr (Ptr CUChar) -> CInt
              -> Ptr (Ptr CUChar) -> CInt -> IO CInt

foreign import ccall "swr_get_out_samples"
  swr_get_out_samples :: SwrContext -> CInt -> IO CInt

data AudioParams = AudioParams
  { apChannelLayout :: CULong
  , apSampleRate    :: CInt
  , apSampleFormat  :: AVSampleFormat
  }

makeResampler :: AVCodecContext
              -> AudioParams
              -> AudioParams
              -> IO (AVFrame -> IO (), IO (Maybe AVFrame))
makeResampler ctx inParams outParams = do
  swr <- initSwrContext inParams outParams

  frameChan <- newTChanIO

  let writeFrame frame = do
        srcSamples <- getNumSamples frame
        if srcSamples == 0
           then return ()
           else do
            srcRate <- getSampleRate frame
            delay <- swr_get_delay swr (fromIntegral srcRate)
            let dstSamples = av_rescale_rnd
                            (delay + fromIntegral srcSamples)
                            (fromIntegral (apSampleRate outParams))
                            (fromIntegral srcRate) avRoundUp
                srcData = castPtr (hasData frame)
            dstDataPtr <- malloc
            lineSize <- malloc
            dstChannelCount <- av_get_channel_layout_nb_channels
                (apChannelLayout outParams)
            runWithError "Could not alloc samples"
                (av_samples_alloc_array_and_samples dstDataPtr lineSize
                dstChannelCount (fromIntegral dstSamples)
                (apSampleFormat outParams) 0)
            dstData <- peek dstDataPtr
            runWithError "Error converting samples"
                (swr_convert swr nullPtr 0 srcData srcSamples)

            frameSize <- getFrameSize ctx
            let convertLoop = do
                  outSamples <- swr_get_out_samples swr 0
                  if outSamples < frameSize * dstChannelCount
                     then return ()
                     else do
                       frame <- allocAudioFrame ctx
                       outSamples <- swr_convert swr (castPtr $ hasData frame)
                                                     frameSize nullPtr 0

                       atomically $ writeTChan frameChan frame
                       convertLoop

            convertLoop
            free dstData
            free lineSize
            return ()

      allocAudioFrame :: AVCodecContext -> IO AVFrame
      allocAudioFrame ctx = do
        frame <- av_frame_alloc
        when (getPtr frame == nullPtr)
            (error "Error allocating an audio frame")

        setFormat frame . getSampleFormatInt =<< getSampleFormat ctx
        setChannelLayout frame =<< getChannelLayout ctx
        setSampleRate frame =<< getSampleRate ctx
        fs <- (do fs <- getFrameSize ctx
                  if fs == 0
                    then return 1000
                    else return fs)
        setNumSamples frame fs

        runWithError "Error allocating an audio buffer"
                        (av_frame_get_buffer frame 0)
        return frame

      readFrame = do
        mFrame <- atomically $ tryReadTChan frameChan
        case mFrame of
          Nothing -> return Nothing
          Just _  -> return mFrame

  return (writeFrame, readFrame)

initSwrContext :: AudioParams -> AudioParams -> IO SwrContext
initSwrContext inParams outParams = do
  swr <- swr_alloc
  when (getPtr swr == nullPtr) (error "Could not allocate resampler context")
  let set_int str i = do
        cStr <- newCString str
        av_opt_set_int (getPtr swr) cStr (fromIntegral i) 0
        free cStr
      set_sample_fmt str fmt = do
        cStr <- newCString str
        av_opt_set_sample_fmt (getPtr swr) cStr fmt 0
        free cStr

  -- set_int "in_channel_count" (aoChannelCount inParams)
  set_int "in_channel_layout" (apChannelLayout inParams)
  set_int "in_sample_rate" (apSampleRate inParams)
  set_sample_fmt "in_sample_fmt" (apSampleFormat inParams)
  -- set_int "out_channel_count" (aoChannelCount outParams)
  set_int "out_channel_layout" (apChannelLayout inParams)
  set_int "out_sample_rate" (apSampleRate outParams)
  set_sample_fmt "out_sample_fmt" (apSampleFormat outParams)

  void $ runWithError "Failed to initialize the resampling context" (swr_init swr)

  let get_int str = do
        cStr <- newCString str
        p <- malloc
        r <- av_opt_get_int (getPtr swr) cStr 0 p
        v <- peek p
        free p
        return v
      get_sample_fmt str = do
        cStr <- newCString str
        p <- malloc
        r <- av_opt_get_sample_fmt (getPtr swr) cStr 0 p
        fmt <- peek p
        free p
        return fmt

  return swr
