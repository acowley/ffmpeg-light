-- | Helpers for dumping information about codecs to stdout.
module Codec.FFmpeg.Internal.Debug where
import Codec.FFmpeg.Types
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>

-- | FFmpeg's built-in format debug utlity.
foreign import ccall "av_dump_format"
  av_dump_format :: AVFormatContext -> CInt -> CString -> CInt -> IO ()

-- | Print the short name, long name, and ID of a codec.
debugCodec :: AVCodec -> IO ()
debugCodec cod = do
  longName <- getLongName cod >>= peekCString
  shortName <- getName cod >>= peekCString
  cid <- getCodecID cod
  putStrLn $ "Codec short_name = " ++ show shortName
  putStrLn $ "Codec long_name = " ++ show longName
  putStrLn $ "Codec ID = " ++ show cid

-- | Print various codec settings.
debugCodecContext :: AVCodecContext -> IO()
debugCodecContext (AVCodecContext p) = do
  putStrLn "*** AVCodecContext dump:"
  (#peek AVCodecContext, profile) p >>= si "profile"
  (#peek AVCodecContext, flags) p >>= si "flags"
  (#peek AVCodecContext, flags2) p >>= si "flags2"
  (#peek AVCodecContext, gop_size) p >>= si "gop_size"
  (#peek AVCodecContext, bit_rate) p >>= si "bit_rate"
  (#peek AVCodecContext, max_b_frames) p >>= si "max_b_frames"
  (#peek AVCodecContext, b_frame_strategy) p >>= si "b_frame_strategy"
  (#peek AVCodecContext, qmin) p >>= si "qmin"
  (#peek AVCodecContext, qmax) p >>= si "qmax"
  (#peek AVCodecContext, me_cmp) p >>= si "me_cmp"
  (#peek AVCodecContext, me_range) p >>= si "me_range"
  putStrLn ""
  where si msg = putStrLn . ((msg++" = ")++) . show :: CInt -> IO ()
