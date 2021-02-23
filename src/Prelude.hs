module Prelude
    ( module Relude
    , module Data.Aeson
    , module Conduit
    , readJSONFile
    , writeJSONFile
    , om
    )
where

import           Relude

import           System.IO.Error                ( userError )
import           Control.Monad.Catch            ( MonadThrow
                                                , throwM
                                                )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , Result(..)
                                                , eitherDecodeStrict'
                                                , fromJSON
                                                , fromEncoding
                                                , toEncoding
                                                , json
                                                , Value
                                                )
import           Conduit                        ( builderToByteString
                                                , sinkFileBS
                                                , MonadUnliftIO
                                                )
import           Data.Conduit                   ( ConduitM
                                                , runConduitRes
                                                , (.|)
                                                , yield
                                                , catchC
                                                )
import           Data.Conduit.Attoparsec        ( sinkParser
                                                , ParseError(..)
                                                )
import           Data.Conduit.Binary            ( sourceFile )



om :: Monad m => (a -> b -> m c) -> m a -> b -> m c
om f m = (m >>=) . flip f


sinkFromJSON
    :: (MonadUnliftIO m, MonadThrow m, FromJSON a) => ConduitM ByteString o m a
sinkFromJSON = do
    value <- sinkParser json `catchC` \e -> do
        let _ = (e :: ParseError)
        throwM $ userError "couldNotRead"
    case fromJSON value of
        Error   e -> throwM $ userError e
        Success x -> return x

readJSONFile :: (MonadThrow m, MonadIO m, FromJSON a) => FilePath -> m a
readJSONFile fp = liftIO $ runConduitRes $ sourceFile fp .| sinkFromJSON


writeJSONFile :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJSONFile fp x =
    liftIO
        $  runConduitRes
        $  yield (fromEncoding $ toEncoding x)
        .| builderToByteString
        .| sinkFileBS fp
