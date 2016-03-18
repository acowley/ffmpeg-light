{-# LANGUAGE ForeignFunctionInterface, FlexibleContexts #-}
-- | Interface to initialize FFmpeg, decode video files, encode video
-- files, and convert decoded image frames to JuicyPixels images.
module Codec.FFmpeg (-- * Initialization
                     initFFmpeg, setLogLevel,
                     -- * Decoding
                     imageReader, imageReaderTime,
                     imageReaderT, imageReaderTimeT,
                     -- * Encoding
                     EncodingParams(..), defaultParams, imageWriter,
                     -- * Types and Enums
                     module Codec.FFmpeg.Types, 
                     module Codec.FFmpeg.Enums
                     )where
import Codec.FFmpeg.Encode
import Codec.FFmpeg.Enums
import Codec.FFmpeg.Juicy
import Codec.FFmpeg.Types
import Foreign.C.Types (CInt(..))

foreign import ccall "av_register_all" av_register_all :: IO ()
foreign import ccall "avdevice_register_all" avdevice_register_all :: IO ()

-- foreign import ccall "avcodec_register_all" avcodec_register_all :: IO (

foreign import ccall "av_log_set_level" av_log_set_level :: CInt -> IO ()

-- | Log output is sent to stderr.
setLogLevel :: LogLevel -> IO ()
setLogLevel (LogLevel l) = av_log_set_level l

-- | Initialize FFmpeg by registering all known codecs. This /must/ be
-- called before using other FFmpeg functions. The debug level is
-- initially set to @quiet@. If you would like the standard ffmpeg
-- debug level, call @setLogLevel avLogInfo@ after @initFFmpeg@.
initFFmpeg :: IO ()
initFFmpeg = av_register_all >> avdevice_register_all >> setLogLevel avLogQuiet
