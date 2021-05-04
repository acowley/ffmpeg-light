{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codec.FFmpeg.Enums where
import Data.Bits (Bits)
import Foreign.C.Types
import Foreign.Storable (Storable)

#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavutil/avutil.h>
#include <libavutil/pixfmt.h>
#include <libavutil/mathematics.h>
#include <libswscale/swscale.h>
#include <libswresample/swresample.h>
#include "nameCompat.h"

newtype AVMediaType = AVMediaType CInt deriving (Eq, Storable)
#enum AVMediaType,AVMediaType \
 , AVMEDIA_TYPE_VIDEO\
 , AVMEDIA_TYPE_AUDIO\
 , AVMEDIA_TYPE_DATA\
 , AVMEDIA_TYPE_SUBTITLE\
 , AVMEDIA_TYPE_ATTACHMENT\
 , AVMEDIA_TYPE_NB

newtype AVPixelFormat = AVPixelFormat CInt deriving (Eq, Storable)
#enum AVPixelFormat,AVPixelFormat \
 , AV_PIX_FMT_NONE\
 , AV_PIX_FMT_RGB24\
 , AV_PIX_FMT_RGBA\
 , AV_PIX_FMT_BGRA\
 , AV_PIX_FMT_Y400A\
 , AV_PIX_FMT_RGB32\
 , AV_PIX_FMT_RGB32_1\
 , AV_PIX_FMT_BGR32\
 , AV_PIX_FMT_BGR32_1\
 , AV_PIX_FMT_RGB8\
 , AV_PIX_FMT_BGR8\
 , AV_PIX_FMT_RGB4_BYTE\
 , AV_PIX_FMT_BGR4_BYTE\
 , AV_PIX_FMT_GRAY8\
 , AV_PIX_FMT_GRAY16\
 , AV_PIX_FMT_GRAY8A\
 , AV_PIX_FMT_PAL8\
 , AV_PIX_FMT_RGB565\
 , AV_PIX_FMT_RGB555\
 , AV_PIX_FMT_YUV420P\
 , AV_PIX_FMT_YUV420P9\
 , AV_PIX_FMT_YUV420P10\
 , AV_PIX_FMT_YUV420P12\
 , AV_PIX_FMT_YUV422P12\
 , AV_PIX_FMT_YUV444P12\
 , AV_PIX_FMT_YUV420P14\
 , AV_PIX_FMT_YUV422P14\
 , AV_PIX_FMT_YUV444P14\
 , AV_PIX_FMT_YUV420P16\
 , AV_PIX_FMT_YUV422P16\
 , AV_PIX_FMT_YUV444P16\
 , AV_PIX_FMT_RGBA64\
 , AV_PIX_FMT_BGRA64

instance Show AVPixelFormat where
  show x
    | x == avPixFmtRgb24 = "AV_PIX_FMT_RGB24"
    | x == avPixFmtYuv420p = "AV_PIX_FMT_Y420P"
    | x == avPixFmtYuv422p12 = "AV_PIX_FMTYUV422P12"
    | x == avPixFmtYuv420p14 = "AV_PIX_FMTYUV422P12"
    | otherwise = let AVPixelFormat y = x
                  in "Other pixel format: "++show y

newtype AVCodecID = AVCodecID CInt deriving (Eq, Show, Storable)
#enum AVCodecID,AVCodecID \
 , AV_CODEC_ID_NONE\
 , AV_CODEC_ID_MPEG1VIDEO\
 , AV_CODEC_ID_MPEG2VIDEO\
 , AV_CODEC_ID_H261\
 , AV_CODEC_ID_H263\
 , AV_CODEC_ID_RV10\
 , AV_CODEC_ID_RV20\
 , AV_CODEC_ID_MJPEG\
 , AV_CODEC_ID_MJPEGB\
 , AV_CODEC_ID_LJPEG\
 , AV_CODEC_ID_SP5X\
 , AV_CODEC_ID_JPEGLS\
 , AV_CODEC_ID_MPEG4\
 , AV_CODEC_ID_RAWVIDEO\
 , AV_CODEC_ID_MSMPEG4V1\
 , AV_CODEC_ID_MSMPEG4V2\
 , AV_CODEC_ID_MSMPEG4V3\
 , AV_CODEC_ID_WMV1\
 , AV_CODEC_ID_WMV2\
 , AV_CODEC_ID_H263P\
 , AV_CODEC_ID_H263I\
 , AV_CODEC_ID_FLV1\
 , AV_CODEC_ID_SVQ1\
 , AV_CODEC_ID_SVQ3\
 , AV_CODEC_ID_DVVIDEO\
 , AV_CODEC_ID_HUFFYUV\
 , AV_CODEC_ID_CYUV\
 , AV_CODEC_ID_H264\
 , AV_CODEC_ID_INDEO3\
 , AV_CODEC_ID_VP3\
 , AV_CODEC_ID_THEORA\
 , AV_CODEC_ID_ASV1\
 , AV_CODEC_ID_ASV2\
 , AV_CODEC_ID_FFV1\
 , AV_CODEC_ID_4XM\
 , AV_CODEC_ID_VCR1\
 , AV_CODEC_ID_CLJR\
 , AV_CODEC_ID_MDEC\
 , AV_CODEC_ID_ROQ\
 , AV_CODEC_ID_INTERPLAY_VIDEO\
 , AV_CODEC_ID_XAN_WC3\
 , AV_CODEC_ID_XAN_WC4\
 , AV_CODEC_ID_RPZA\
 , AV_CODEC_ID_CINEPAK\
 , AV_CODEC_ID_WS_VQA\
 , AV_CODEC_ID_MSRLE\
 , AV_CODEC_ID_MSVIDEO1\
 , AV_CODEC_ID_IDCIN\
 , AV_CODEC_ID_8BPS\
 , AV_CODEC_ID_SMC\
 , AV_CODEC_ID_FLIC\
 , AV_CODEC_ID_TRUEMOTION1\
 , AV_CODEC_ID_VMDVIDEO\
 , AV_CODEC_ID_MSZH\
 , AV_CODEC_ID_ZLIB\
 , AV_CODEC_ID_QTRLE\
 , AV_CODEC_ID_TSCC\
 , AV_CODEC_ID_ULTI\
 , AV_CODEC_ID_QDRAW\
 , AV_CODEC_ID_VIXL\
 , AV_CODEC_ID_QPEG\
 , AV_CODEC_ID_PNG\
 , AV_CODEC_ID_PPM\
 , AV_CODEC_ID_PBM\
 , AV_CODEC_ID_PGM\
 , AV_CODEC_ID_PGMYUV\
 , AV_CODEC_ID_PAM\
 , AV_CODEC_ID_FFVHUFF\
 , AV_CODEC_ID_RV30\
 , AV_CODEC_ID_RV40\
 , AV_CODEC_ID_VC1\
 , AV_CODEC_ID_WMV3\
 , AV_CODEC_ID_LOCO\
 , AV_CODEC_ID_WNV1\
 , AV_CODEC_ID_AASC\
 , AV_CODEC_ID_INDEO2\
 , AV_CODEC_ID_FRAPS\
 , AV_CODEC_ID_TRUEMOTION2\
 , AV_CODEC_ID_BMP\
 , AV_CODEC_ID_CSCD\
 , AV_CODEC_ID_MMVIDEO\
 , AV_CODEC_ID_ZMBV\
 , AV_CODEC_ID_AVS\
 , AV_CODEC_ID_SMACKVIDEO\
 , AV_CODEC_ID_NUV\
 , AV_CODEC_ID_KMVC\
 , AV_CODEC_ID_FLASHSV\
 , AV_CODEC_ID_CAVS\
 , AV_CODEC_ID_JPEG2000\
 , AV_CODEC_ID_VMNC\
 , AV_CODEC_ID_VP5\
 , AV_CODEC_ID_VP6\
 , AV_CODEC_ID_VP6F\
 , AV_CODEC_ID_TARGA\
 , AV_CODEC_ID_DSICINVIDEO\
 , AV_CODEC_ID_TIERTEXSEQVIDEO\
 , AV_CODEC_ID_TIFF\
 , AV_CODEC_ID_GIF\
 , AV_CODEC_ID_DXA\
 , AV_CODEC_ID_DNXHD\
 , AV_CODEC_ID_THP\
 , AV_CODEC_ID_SGI\
 , AV_CODEC_ID_C93\
 , AV_CODEC_ID_BETHSOFTVID\
 , AV_CODEC_ID_PTX\
 , AV_CODEC_ID_TXD\
 , AV_CODEC_ID_VP6A\
 , AV_CODEC_ID_AMV\
 , AV_CODEC_ID_VB\
 , AV_CODEC_ID_PCX\
 , AV_CODEC_ID_SUNRAST\
 , AV_CODEC_ID_INDEO4\
 , AV_CODEC_ID_INDEO5\
 , AV_CODEC_ID_MIMIC\
 , AV_CODEC_ID_RL2\
 , AV_CODEC_ID_ESCAPE124\
 , AV_CODEC_ID_DIRAC\
 , AV_CODEC_ID_BFI\
 , AV_CODEC_ID_CMV\
 , AV_CODEC_ID_MOTIONPIXELS\
 , AV_CODEC_ID_TGV\
 , AV_CODEC_ID_TGQ\
 , AV_CODEC_ID_TQI\
 , AV_CODEC_ID_AURA\
 , AV_CODEC_ID_AURA2\
 , AV_CODEC_ID_V210X\
 , AV_CODEC_ID_TMV\
 , AV_CODEC_ID_V210\
 , AV_CODEC_ID_DPX\
 , AV_CODEC_ID_MAD\
 , AV_CODEC_ID_FRWU\
 , AV_CODEC_ID_FLASHSV2\
 , AV_CODEC_ID_CDGRAPHICS\
 , AV_CODEC_ID_R210\
 , AV_CODEC_ID_ANM\
 , AV_CODEC_ID_BINKVIDEO\
 , AV_CODEC_ID_IFF_ILBM\
 , AV_CODEC_ID_KGV1\
 , AV_CODEC_ID_YOP\
 , AV_CODEC_ID_VP8\
 , AV_CODEC_ID_PICTOR\
 , AV_CODEC_ID_ANSI\
 , AV_CODEC_ID_A64_MULTI\
 , AV_CODEC_ID_A64_MULTI5\
 , AV_CODEC_ID_R10K\
 , AV_CODEC_ID_MXPEG\
 , AV_CODEC_ID_LAGARITH\
 , AV_CODEC_ID_PRORES\
 , AV_CODEC_ID_JV\
 , AV_CODEC_ID_DFA\
 , AV_CODEC_ID_WMV3IMAGE\
 , AV_CODEC_ID_VC1IMAGE\
 , AV_CODEC_ID_UTVIDEO\
 , AV_CODEC_ID_BMV_VIDEO\
 , AV_CODEC_ID_VBLE\
 , AV_CODEC_ID_DXTORY\
 , AV_CODEC_ID_V410\
 , AV_CODEC_ID_XWD\
 , AV_CODEC_ID_CDXL\
 , AV_CODEC_ID_XBM\
 , AV_CODEC_ID_ZEROCODEC\
 , AV_CODEC_ID_MSS1\
 , AV_CODEC_ID_MSA1\
 , AV_CODEC_ID_TSCC2\
 , AV_CODEC_ID_MTS2\
 , AV_CODEC_ID_CLLC\
 , AV_CODEC_ID_MSS2\
 , AV_CODEC_ID_VP9\
 , AV_CODEC_ID_AIC\
 , AV_CODEC_ID_ESCAPE130\
 , AV_CODEC_ID_G2M\
 , AV_CODEC_ID_WEBP\
 , AV_CODEC_ID_HNM4_VIDEO\
 , AV_CODEC_ID_HEVC\
 , AV_CODEC_ID_MP2\
 , AV_CODEC_ID_MP3\
 , AV_CODEC_ID_AAC\
 , AV_CODEC_ID_AC3\
 , AV_CODEC_ID_DTS\
 , AV_CODEC_ID_VORBIS\
 , AV_CODEC_ID_DVAUDIO\
 , AV_CODEC_ID_WMAV1\
 , AV_CODEC_ID_WMAV2\
 , AV_CODEC_ID_MACE3\
 , AV_CODEC_ID_MACE6\
 , AV_CODEC_ID_VMDAUDIO\
 , AV_CODEC_ID_FLAC\
 , AV_CODEC_ID_MP3ADU\
 , AV_CODEC_ID_MP3ON4\
 , AV_CODEC_ID_SHORTEN\
 , AV_CODEC_ID_ALAC\
 , AV_CODEC_ID_WESTWOOD_SND1\
 , AV_CODEC_ID_GSM\
 , AV_CODEC_ID_QDM2\
 , AV_CODEC_ID_COOK\
 , AV_CODEC_ID_TRUESPEECH\
 , AV_CODEC_ID_TTA\
 , AV_CODEC_ID_SMACKAUDIO\
 , AV_CODEC_ID_QCELP\
 , AV_CODEC_ID_WAVPACK\
 , AV_CODEC_ID_DSICINAUDIO\
 , AV_CODEC_ID_IMC\
 , AV_CODEC_ID_MUSEPACK7\
 , AV_CODEC_ID_MLP\
 , AV_CODEC_ID_GSM_MS\
 , AV_CODEC_ID_ATRAC3\
 , AV_CODEC_ID_APE\
 , AV_CODEC_ID_NELLYMOSER\
 , AV_CODEC_ID_MUSEPACK8\
 , AV_CODEC_ID_SPEEX\
 , AV_CODEC_ID_WMAVOICE\
 , AV_CODEC_ID_WMAPRO\
 , AV_CODEC_ID_WMALOSSLESS\
 , AV_CODEC_ID_ATRAC3P\
 , AV_CODEC_ID_EAC3\
 , AV_CODEC_ID_SIPR\
 , AV_CODEC_ID_MP1\
 , AV_CODEC_ID_TWINVQ\
 , AV_CODEC_ID_TRUEHD\
 , AV_CODEC_ID_MP4ALS\
 , AV_CODEC_ID_ATRAC1\
 , AV_CODEC_ID_BINKAUDIO_RDFT\
 , AV_CODEC_ID_BINKAUDIO_DCT\
 , AV_CODEC_ID_AAC_LATM\
 , AV_CODEC_ID_QDMC\
 , AV_CODEC_ID_CELT\
 , AV_CODEC_ID_G723_1\
 , AV_CODEC_ID_G729\
 , AV_CODEC_ID_8SVX_EXP\
 , AV_CODEC_ID_8SVX_FIB\
 , AV_CODEC_ID_BMV_AUDIO\
 , AV_CODEC_ID_RALF\
 , AV_CODEC_ID_IAC\
 , AV_CODEC_ID_ILBC\
 , AV_CODEC_ID_OPUS\
 , AV_CODEC_ID_COMFORT_NOISE\
 , AV_CODEC_ID_TAK\
 , AV_CODEC_ID_METASOUND\
 , AV_CODEC_ID_PAF_AUDIO\
 , AV_CODEC_ID_ON2AVC\
 , AV_CODEC_ID_DSS_SP\
 , AV_CODEC_ID_FFWAVESYNTH\
 , AV_CODEC_ID_SONIC\
 , AV_CODEC_ID_SONIC_LS\
 , AV_CODEC_ID_EVRC\
 , AV_CODEC_ID_SMV\
 , AV_CODEC_ID_DSD_LSBF\
 , AV_CODEC_ID_DSD_MSBF\
 , AV_CODEC_ID_DSD_LSBF_PLANAR\
 , AV_CODEC_ID_DSD_MSBF_PLANAR\
 , AV_CODEC_ID_4GV\
 , AV_CODEC_ID_INTERPLAY_ACM\
 , AV_CODEC_ID_XMA1\
 , AV_CODEC_ID_XMA2\
 , AV_CODEC_ID_DST


newtype SwsAlgorithm = SwsAlgorithm CUInt deriving (Eq, Show, Storable)
#enum SwsAlgorithm,SwsAlgorithm \
 , SWS_FAST_BILINEAR\
 , SWS_BILINEAR\
 , SWS_BICUBIC\
 , SWS_X\
 , SWS_POINT\
 , SWS_AREA\
 , SWS_BICUBLIN\
 , SWS_GAUSS\
 , SWS_SINC\
 , SWS_LANCZOS\
 , SWS_SPLINE

newtype FFProfile = FFProfile CInt deriving (Eq, Storable)
#enum FFProfile, FFProfile \
 , FF_PROFILE_AAC_MAIN\
 , FF_PROFILE_AAC_LOW\
 , FF_PROFILE_AAC_SSR\
 , FF_PROFILE_AAC_LTP\
 , FF_PROFILE_AAC_HE\
 , FF_PROFILE_AAC_HE_V2\
 , FF_PROFILE_AAC_LD\
 , FF_PROFILE_AAC_ELD\
 , FF_PROFILE_MPEG2_AAC_LOW\
 , FF_PROFILE_MPEG2_AAC_HE\
 , FF_PROFILE_DTS\
 , FF_PROFILE_DTS_ES\
 , FF_PROFILE_DTS_96_24\
 , FF_PROFILE_DTS_HD_HRA\
 , FF_PROFILE_DTS_HD_MA\
 , FF_PROFILE_MPEG2_422\
 , FF_PROFILE_MPEG2_HIGH\
 , FF_PROFILE_MPEG2_SS\
 , FF_PROFILE_MPEG2_SNR_SCALABLE\
 , FF_PROFILE_MPEG2_MAIN\
 , FF_PROFILE_MPEG2_SIMPLE\
 , FF_PROFILE_H264_CONSTRAINED\
 , FF_PROFILE_H264_INTRA\
 , FF_PROFILE_H264_BASELINE\
 , FF_PROFILE_H264_CONSTRAINED_BASELINE\
 , FF_PROFILE_H264_MAIN\
 , FF_PROFILE_H264_EXTENDED\
 , FF_PROFILE_H264_HIGH\
 , FF_PROFILE_H264_HIGH_10\
 , FF_PROFILE_H264_HIGH_10_INTRA\
 , FF_PROFILE_H264_HIGH_422\
 , FF_PROFILE_H264_HIGH_422_INTRA\
 , FF_PROFILE_H264_HIGH_444\
 , FF_PROFILE_H264_HIGH_444_PREDICTIVE\
 , FF_PROFILE_H264_HIGH_444_INTRA\
 , FF_PROFILE_H264_CAVLC_444\
 , FF_PROFILE_VC1_SIMPLE\
 , FF_PROFILE_VC1_MAIN\
 , FF_PROFILE_VC1_COMPLEX\
 , FF_PROFILE_VC1_ADVANCED\
 , FF_PROFILE_MPEG4_SIMPLE\
 , FF_PROFILE_MPEG4_SIMPLE_SCALABLE\
 , FF_PROFILE_MPEG4_CORE\
 , FF_PROFILE_MPEG4_MAIN\
 , FF_PROFILE_MPEG4_N_BIT\
 , FF_PROFILE_MPEG4_SCALABLE_TEXTURE\
 , FF_PROFILE_MPEG4_SIMPLE_FACE_ANIMATION\
 , FF_PROFILE_MPEG4_BASIC_ANIMATED_TEXTURE\
 , FF_PROFILE_MPEG4_HYBRID\
 , FF_PROFILE_MPEG4_ADVANCED_REAL_TIME\
 , FF_PROFILE_MPEG4_CORE_SCALABLE\
 , FF_PROFILE_MPEG4_ADVANCED_CODING\
 , FF_PROFILE_MPEG4_ADVANCED_CORE\
 , FF_PROFILE_MPEG4_ADVANCED_SCALABLE_TEXTURE\
 , FF_PROFILE_MPEG4_SIMPLE_STUDIO\
 , FF_PROFILE_MPEG4_ADVANCED_SIMPLE

newtype AVIOFlag = AVIOFlag CInt deriving (Eq, Storable)
#enum AVIOFlag, AVIOFlag \
 , AVIO_FLAG_READ\
 , AVIO_FLAG_WRITE\
 , AVIO_FLAG_READ_WRITE\
 , AVIO_FLAG_NONBLOCK\
 , AVIO_FLAG_DIRECT

newtype AVRoundMode = AVRoundMode CInt deriving (Eq, Storable)
#enum AVRoundMode, AVRoundMode \
 , AV_ROUND_ZERO\
 , AV_ROUND_INF\
 , AV_ROUND_DOWN\
 , AV_ROUND_UP\
 , AV_ROUND_NEAR_INF\
 , AV_ROUND_PASS_MINMAX

newtype CodecFlag = CodecFlag CInt deriving (Eq, Bits, Storable)
#if LIBAVCODEC_VERSION_MAJOR < 57
#enum CodecFlag, CodecFlag \
 , CODEC_FLAG_UNALIGNED\
 , CODEC_FLAG_QSCALE\
 , CODEC_FLAG_4MV\
 , CODEC_FLAG_OUTPUT_CORRUPT\
 , CODEC_FLAG_QPEL\
 , CODEC_FLAG_GMC\
 , CODEC_FLAG_MV0\
 , CODEC_FLAG_INPUT_PRESERVED\
 , CODEC_FLAG_PASS1\
 , CODEC_FLAG_PASS2\
 , CODEC_FLAG_GRAY\
 , CODEC_FLAG_EMU_EDGE\
 , CODEC_FLAG_PSNR\
 , CODEC_FLAG_TRUNCATED\
 , CODEC_FLAG_NORMALIZE_AQP\
 , CODEC_FLAG_INTERLACED_DCT\
 , CODEC_FLAG_LOW_DELAY\
 , CODEC_FLAG_GLOBAL_HEADER\
 , CODEC_FLAG_BITEXACT\
 , CODEC_FLAG_AC_PRED\
 , CODEC_FLAG_LOOP_FILTER\
 , CODEC_FLAG_INTERLACED_ME\
 , CODEC_FLAG_CLOSED_GOP
#else
#enum CodecFlag, CodecFlag \
 , AV_CODEC_FLAG_UNALIGNED\
 , AV_CODEC_FLAG_QSCALE\
 , AV_CODEC_FLAG_4MV\
 , AV_CODEC_FLAG_OUTPUT_CORRUPT\
 , AV_CODEC_FLAG_QPEL\
 , AV_CODEC_FLAG_PASS1\
 , AV_CODEC_FLAG_PASS2\
 , AV_CODEC_FLAG_LOOP_FILTER\
 , AV_CODEC_FLAG_GRAY\
 , AV_CODEC_FLAG_PSNR\
 , AV_CODEC_FLAG_TRUNCATED\
 , AV_CODEC_FLAG_INTERLACED_DCT\
 , AV_CODEC_FLAG_LOW_DELAY\
 , AV_CODEC_FLAG_GLOBAL_HEADER\
 , AV_CODEC_FLAG_BITEXACT\
 , AV_CODEC_FLAG_AC_PRED\
 , AV_CODEC_FLAG_INTERLACED_ME\
 , AV_CODEC_FLAG_CLOSED_GOP
#endif

newtype FormatFlag = FormatFlag CInt deriving (Eq, Bits, Storable)
#if LIBAVCODEC_VERSION_MAJOR < 57
#enum FormatFlag, FormatFlag \
 , AVFMT_NOFILE\
 , AVFMT_NEEDNUMBER\
 , AVFMT_RAWPICTURE\
 , AVFMT_GLOBALHEADER\
 , AVFMT_NOTIMESTAMPS\
 , AVFMT_VARIABLE_FPS\
 , AVFMT_NODIMENSIONS\
 , AVFMT_NOSTREAMS\
 , AVFMT_ALLOW_FLUSH\
 , AVFMT_TS_NONSTRICT
#else
#enum FormatFlag, FormatFlag \
 , AVFMT_NOFILE\
 , AVFMT_NEEDNUMBER\
 , AVFMT_GLOBALHEADER\
 , AVFMT_NOTIMESTAMPS\
 , AVFMT_VARIABLE_FPS\
 , AVFMT_NODIMENSIONS\
 , AVFMT_NOSTREAMS\
 , AVFMT_NOBINSEARCH\
 , AVFMT_NOGENSEARCH\
 , AVFMT_NO_BYTE_SEEK\
 , AVFMT_ALLOW_FLUSH\
 , AVFMT_TS_NONSTRICT\
 , AVFMT_TS_NEGATIVE\
 , AVFMT_SEEK_TO_PTS
#endif

newtype PacketFlag = PacketFlag CInt deriving (Eq, Bits, Storable)
#enum PacketFlag, PacketFlag \
 , AV_PKT_FLAG_KEY\
 , AV_PKT_FLAG_CORRUPT

newtype LogLevel = LogLevel CInt deriving (Eq, Bits, Storable)
#enum LogLevel, LogLevel \
 , AV_LOG_QUIET\
 , AV_LOG_PANIC\
 , AV_LOG_FATAL\
 , AV_LOG_ERROR\
 , AV_LOG_WARNING\
 , AV_LOG_INFO\
 , AV_LOG_VERBOSE\
 , AV_LOG_DEBUG\
 , AV_LOG_TRACE\
 , AV_LOG_MAX_OFFSET

newtype AVError = AVError CInt deriving (Eq, Bits, Storable)
#enum AVError, AVError \
 , AVERROR_EOF \
 , AVERROR_EXIT

newtype AVSampleFormat = AVSampleFormat CInt deriving (Eq, Bits, Storable)
#enum AVSampleFormat, AVSampleFormat \
 , AV_SAMPLE_FMT_NONE\
 , AV_SAMPLE_FMT_U8\
 , AV_SAMPLE_FMT_S16\
 , AV_SAMPLE_FMT_S32\
 , AV_SAMPLE_FMT_FLT\
 , AV_SAMPLE_FMT_DBL\
 , AV_SAMPLE_FMT_U8P\
 , AV_SAMPLE_FMT_S16P\
 , AV_SAMPLE_FMT_S32P\
 , AV_SAMPLE_FMT_FLTP\
 , AV_SAMPLE_FMT_DBLP\
 , AV_SAMPLE_FMT_NB

getSampleFormatInt :: AVSampleFormat -> CInt
getSampleFormatInt (AVSampleFormat i) = i
