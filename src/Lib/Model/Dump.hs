{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Dump
    ( Dump(..)
    , getDump
    , writeDump
    , initalState
    , unDump
    )
where

import           Lib.Model.Data
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )


newtype Dump = Dump { _unDump :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Dump



getDump :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Dump
getDump = readJSONFile

writeDump :: (MonadIO m, MonadThrow m) => FilePath -> Dump -> m ()
writeDump = writeJSONFile

initalState :: Data String Dump
initalState = NotAsked

