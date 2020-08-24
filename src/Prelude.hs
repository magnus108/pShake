module Prelude
    ( module Relude
    , module Data.Aeson
    , module Conduit
    , readJSONFile
    , writeJSONFile
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
                                                )
import           Data.Conduit                   ( ConduitM
                                                , runConduitRes
                                                , (.|)
                                                , yield
                                                )
import           Data.Conduit.Attoparsec        ( sinkParser )
import           Data.Conduit.Binary            ( sourceFile )


sinkFromJSON :: (MonadThrow m, FromJSON a) => ConduitM ByteString o m a
sinkFromJSON = do
    value <- sinkParser json
    case fromJSON value of
        Error   e -> throwM $ userError e
        Success x -> return x


readJSONFile :: (MonadIO m, FromJSON a) => FilePath -> m a
readJSONFile fp = liftIO $ runConduitRes $ sourceFile fp .| sinkFromJSON


writeJSONFile :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeJSONFile fp x =
    liftIO
        $  runConduitRes
        $  yield (fromEncoding $ toEncoding x)
        .| builderToByteString
        .| sinkFileBS fp
