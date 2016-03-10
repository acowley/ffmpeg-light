
{-# LANGUAGE
    ForeignFunctionInterface,
    GeneralizedNewtypeDeriving
    #-}

module Codec.FFmpeg.Probe (
    -- * Files
    withAvFile, nbStreams, formatName, formatMetadata, duration,

    -- * Streams
    AvStreamT, withStream, codecContext, codecName,
    codecMediaTypeName, streamBitrate, streamMetadata,
    codec,

    -- * Dictionaries
    dictFoldM_
    ) where

import Control.Applicative ( Applicative )
import Control.Monad.Catch ( MonadMask, finally )
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Data.Int ( Int64 )
import Foreign.C.String ( CString, peekCString, withCString )
import Foreign.C.Types ( CInt(..) )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, nullPtr )
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
        , MonadTrans
        )

withAvFile :: (MonadMask m, MonadIO m) => String -> AvFormat m a -> m a
withAvFile fn f = do
    ectx <- runEitherT $ openFile fn
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

duration :: MonadIO m => AvFormat m Int64
duration = ask >>= \ctx -> liftIO $ (#peek AVFormatContext, duration) (getPtr ctx)

formatMetadata :: MonadIO m => AvFormat m AVDictionary
formatMetadata = ask >>= liftIO . (#peek AVFormatContext, metadata) . getPtr

-------------------------------------------------------------------------------
-- stream - level stuff
-------------------------------------------------------------------------------

newtype AvStreamT m a = AvStreamT { unAvStreamT :: ReaderT AVStream (m) a }
    deriving
        ( Applicative
        , Functor
        , Monad
        , MonadIO
        , MonadReader AVStream
        , MonadTrans
        )

withStream :: (MonadIO m) => Int -> AvStreamT (AvFormat m) a -> AvFormat m a
withStream sid f = nbStreams >>= \ns -> if sid >= ns
    then error $ show sid ++ " >= " ++ show ns
    else do
        ctx <- ask
        streams <- liftIO $ (#peek AVFormatContext, streams) (getPtr ctx)
        liftIO (peekElemOff streams sid) >>= runReaderT (unAvStreamT f)

codecContext :: MonadIO m => AvStreamT m (Maybe AVCodecContext)
codecContext = do
    p <- ask >>= (liftIO . (#peek AVStream, codec) . getPtr)
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

streamMetadata :: MonadIO m => AvStreamT m AVDictionary
streamMetadata = ask >>= liftIO . (#peek AVStream, metadata) . getPtr

-------------------------------------------------------------------------------
-- dictionaries
-------------------------------------------------------------------------------

dictFoldM_
    :: MonadIO m
    => ((String, String) -> m ())
    -> AVDictionary
    -> m ()
dictFoldM_ f d =
    let
        flags = (#const AV_DICT_IGNORE_SUFFIX + AV_DICT_DONT_STRDUP_KEY + AV_DICT_DONT_STRDUP_VAL)
        next ep = do
            e' <- liftIO $ withCString "" $ \s -> av_dict_get d s ep flags
            if (e' == nullPtr)
                then return ()
                else do
                    k <- liftIO $ (#peek AVDictionaryEntry, key) e' >>= peekCString
                    v <- liftIO $ (#peek AVDictionaryEntry, value) e' >>= peekCString
                    f (k, v)
                    next e'
    in do
        -- e <- liftIO $ malloc >>= \m -> poke m nullPtr >> return m
        next nullPtr

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

foreign import ccall "av_dict_get"
  av_dict_get :: AVDictionary -> CString -> Ptr () -> CInt -> IO (Ptr ())
