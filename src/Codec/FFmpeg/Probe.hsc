
{-# LANGUAGE
    ForeignFunctionInterface,
    GeneralizedNewtypeDeriving
    #-}

module Codec.FFmpeg.Probe (
    withAvFile, nbStreams, formatName,

    -- * Streams
    AvStreamT, withStream, codecContext, codecName,
    codecMediaTypeName, streamBitrate, codec
    ) where

import Control.Applicative ( Applicative )
import Control.Monad.Catch ( MonadMask, finally )
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Foreign.C.String ( CString, peekCString )
import Foreign.C.Types ( CInt(..) )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( nullPtr )
import Foreign.Storable

import Codec.FFmpeg.Enums
import Codec.FFmpeg.Decode
import Codec.FFmpeg.Types

#include <libavformat/avformat.h>

-------------------------------------------------------------------------------
-- avformat - level stuff
-------------------------------------------------------------------------------

newtype AvFormat m a = AvFormat { unAvFormat :: ReaderT AVFormatContext m a }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadIO
        , MonadReader AVFormatContext
        )

withAvFile :: (MonadMask m, MonadIO m) => String -> AvFormat m a -> m a
withAvFile fn f = do
    ectx <- runEitherT $ openInput fn
    case ectx of
        Left e    -> liftIO $ fail e
        Right ctx -> finally
            ((liftIO $ avformat_find_stream_info ctx nullPtr) >> runReaderT (unAvFormat f) ctx)
            (liftIO $ with ctx close_input)

nbStreams :: MonadIO m => AvFormat m Int
nbStreams = avToInt $ ask >>= \ctx ->
    liftIO $ (#peek AVFormatContext, nb_streams) (getPtr ctx)

formatName :: MonadIO m => AvFormat m String
formatName = ask >>= \ctx -> liftIO $
    (#peek AVFormatContext, iformat) (getPtr ctx) >>=
    (#peek AVInputFormat, name) >>=
    peekCString

-------------------------------------------------------------------------------
-- stream - level stuff
-------------------------------------------------------------------------------

newtype AvStreamT m a = AvStreamT { unAvStreamT :: ReaderT AVStream (AvFormat m) a }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadIO
        , MonadReader AVStream
        )

withStream :: MonadIO m => Int -> AvStreamT m a -> AvFormat m a
withStream sid f = nbStreams >>= \ns -> if sid >= ns
    then error $ show sid ++ " >= " ++ show ns
    else do
        ctx <- ask
        streams <- liftIO $ (#peek AVFormatContext, streams) (getPtr ctx)
        liftIO (peekElemOff streams sid) >>= runReaderT (unAvStreamT f)

codecContext :: MonadIO m => AvStreamT m (Maybe AVCodecContext)
codecContext = do
    p <- ask >>= (\x -> liftIO $ (#peek AVStream, codec) (getPtr x))
    if (p /= nullPtr)
        then return $ Just $ AVCodecContext p
        else return Nothing

codecMediaTypeName :: MonadIO m => AVCodecContext -> AvStreamT m String
codecMediaTypeName cctx = liftIO $
    (#peek AVCodecContext, codec_type) (getPtr cctx) >>=
    av_get_media_type_string >>=
    peekCString

codec :: MonadIO m => AVCodecContext -> AvStreamT m (Maybe AVCodec)
codec cctx = (liftIO . (#peek AVCodecContext, codec) . getPtr) cctx >>=
    \mc -> if mc == nullPtr
        then return Nothing
        else return $ Just $ AVCodec mc

codecName :: MonadIO m => AVCodecContext -> AvStreamT m String
codecName cctx = liftIO $ getCodecID cctx >>= avcodec_get_name >>= peekCString

streamBitrate :: MonadIO m => AVCodecContext -> AvStreamT m Int
streamBitrate cctx = liftIO $ getBitRate cctx >>= return . fromIntegral

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

avToInt :: Monad m => AvFormat m CInt -> AvFormat m Int
avToInt f = f >>= return . fromIntegral

-------------------------------------------------------------------------------
-- FFI imports
-------------------------------------------------------------------------------

foreign import ccall "av_get_media_type_string"
  av_get_media_type_string :: AVMediaType -> IO CString

foreign import ccall "avcodec_get_name"
  avcodec_get_name :: AVCodecID -> IO CString
