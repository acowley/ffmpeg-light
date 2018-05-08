/* Older versions of libav (and perhaps ffmpeg) used a different
   naming scheme for constants. Since distributions like Ubuntu 12.04
   are locked down with these old versions, we hack in support for the
   newer constant names. */

#if LIBAVUTIL_VERSION_MAJOR < 53
#define AV_PIX_FMT_NONE PIX_FMT_NONE
#define AV_PIX_FMT_RGB24 PIX_FMT_RGB24
#define AV_PIX_FMT_RGBA PIX_FMT_RGBA
#define AV_PIX_FMT_BGRA PIX_FMT_BGRA
#define AV_PIX_FMT_Y400A PIX_FMT_Y400A
#define AV_PIX_FMT_RGB8 -1
#define AV_PIX_FMT_BGR8 -1
#define AV_PIX_FMT_RGB4_BYTE -1
#define AV_PIX_FMT_BGR4_BYTE -1
#define AV_PIX_FMT_GRAY8 PIX_FMT_GRAY8
#define AV_PIX_FMT_GRAY8A -1
#define AV_PIX_FMT_YUV420P PIX_FMT_YUV420P
#define AV_PIX_FMT_YUV420P12 -1
#define AV_PIX_FMT_YUV422P12 -1
#define AV_PIX_FMT_YUV444P12 -1
#define AV_PIX_FMT_YUV420P14 -1
#define AV_PIX_FMT_YUV422P14 -1
#define AV_PIX_FMT_YUV444P14 -1
#define AV_PIX_FMT_RGBA64 -1
#define AV_PIX_FMT_BGRA64 -1
#define AV_PIX_FMT_PAL8 PIX_FMT_PAL8

#define AV_CODEC_ID_H264 CODEC_ID_H264
#define AV_CODEC_ID_THEORA CODEC_ID_THEORA
#define AV_CODEC_ID_MPEG4 CODEC_ID_MPEG4
#define AV_CODEC_ID_MPEG2VIDEO CODEC_ID_MPEG2VIDEO
#define AV_CODEC_ID_GIF CODEC_ID_GIF
#define AV_CODEC_ID_AAC CODEC_ID_AAC
#define AV_CODEC_ID_MP3 CODEC_ID_MP3
#define AV_CODEC_ID_DTS CODEC_ID_DTS
#define AV_CODEC_ID_HEVC -1
#define AV_CODEC_ID_VC1 -1
#define AV_CODEC_ID_RAWVIDEO CODEC_ID_RAWVIDEO

#define FF_PROFILE_MPEG2_AAC_LOW -1
#define FF_PROFILE_MPEG2_AAC_HE -1

#define AVIO_FLAG_DIRECT -1

#define AV_ROUND_PASS_MINMAX -1

#define CODEC_FLAG_UNALIGNED -1
#define CODEC_FLAG_OUTPUT_CORRUPT -1

#define AV_LOG_TRACE 56
#define AV_LOG_MAX_OFFSET (AV_LOG_TRACE - AV_LOG_QUIET)

#endif
