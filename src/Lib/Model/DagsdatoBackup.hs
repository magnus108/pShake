{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.DagsdatoBackup
    ( DagsdatoBackup(..)
    , getDagsdatoBackup
    , writeDagsdatoBackup
    , initalState
    , unDagsdatoBackup
    )
where

import           Lib.Model.Data
import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )


newtype DagsdatoBackup = DagsdatoBackup { _unDagsdatoBackup :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''DagsdatoBackup

getDagsdatoBackup
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m DagsdatoBackup
getDagsdatoBackup = readJSONFile

writeDagsdatoBackup
    :: (MonadIO m, MonadThrow m) => FilePath -> DagsdatoBackup -> m ()
writeDagsdatoBackup = writeJSONFile

initalState :: Data String DagsdatoBackup
initalState = NotAsked
