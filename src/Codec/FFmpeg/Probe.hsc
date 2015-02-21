
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.FFmpeg.Probe (
    withAvFile, nbStreams, formatName,

    -- * Streams
    AvStreamT, withStream, codecContext
    ) where

import Control.Applicative ( Applicative )
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Foreign.C.String ( peekCString )
import Foreign.C.Types ( CInt )
import Foreign.Marshal.Utils ( with )
import Foreign.Ptr ( Ptr, nullPtr )
import Foreign.Storable

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
        Left e    -> error e
        Right ctx -> finally
            (runReaderT (unAvFormat f) ctx)
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

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

avToInt :: Monad m => AvFormat m CInt -> AvFormat m Int
avToInt f = f >>= return . fromIntegral
