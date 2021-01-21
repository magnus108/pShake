{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Doneshooting
    ( Doneshooting(..)
    , getDoneshooting
    , unDoneshooting
    , writeDoneshooting
    , initalState
    )
where

import           Lib.Model.Data
import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )


newtype Doneshooting = Doneshooting { _unDoneshooting :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Doneshooting

getDoneshooting
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Doneshooting
getDoneshooting = readJSONFile

writeDoneshooting
    :: (MonadIO m, MonadThrow m) => FilePath -> Doneshooting -> m ()
writeDoneshooting = writeJSONFile

initalState :: Data String Doneshooting
initalState = NotAsked
