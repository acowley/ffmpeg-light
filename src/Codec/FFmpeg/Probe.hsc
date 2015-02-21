
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.FFmpeg.Probe (
    withAvFile, nbStreams, formatName
    ) where

import Control.Applicative ( Applicative )
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Foreign.C.String ( peekCString )
import Foreign.C.Types ( CInt )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable

import Codec.FFmpeg.Decode
import Codec.FFmpeg.Types

#include <libavformat/avformat.h>

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

avToInt :: Monad m => AvFormat m CInt -> AvFormat m Int
avToInt f = f >>= return . fromIntegral

nbStreams :: MonadIO m => AvFormat m Int
nbStreams = avToInt $ ask >>= \ctx ->
    liftIO $ (#peek AVFormatContext, nb_streams) (getPtr ctx)

formatName :: MonadIO m => AvFormat m String
formatName = ask >>= \ctx -> do
    liftIO $ (#peek AVFormatContext, iformat) (getPtr ctx) >>= (#peek AVInputFormat, name) >>= peekCString
    