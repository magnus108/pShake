{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.DumpDir
    ( DumpDir(..)
    , getDumpDir
    , writeDumpDir
    , initalState
    )
where

import           Lib.Model.Data
import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

newtype DumpDir = DumpDir { unDumpDir :: [FilePath] }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''DumpDir

getDumpDir
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m DumpDir
getDumpDir = readJSONFile

writeDumpDir
    :: (MonadIO m, MonadThrow m) => FilePath -> DumpDir -> m ()
writeDumpDir = writeJSONFile

initalState :: Data String DumpDir
initalState = NotAsked

