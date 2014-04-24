{-# LANGUAGE ForeignFunctionInterface, FlexibleContexts #-}
-- | Interface to initialize FFmpeg, decode video files, encode video
-- files, and convert decoded image frames to JuicyPixels images.
module Codec.FFmpeg (-- * Initialization
                     initFFmpeg, 
                     -- * Decoding
                     imageReader, imageReaderTime,
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

foreign import ccall "av_register_all" av_register_all :: IO ()

-- foreign import ccall "avcodec_register_all" avcodec_register_all :: IO ()

-- | Initialize FFmpeg by registering all known codecs. This /must/
-- be called before using other FFmpeg functions.
initFFmpeg :: IO ()
initFFmpeg = av_register_all

