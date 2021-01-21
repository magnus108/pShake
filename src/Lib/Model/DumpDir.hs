{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.DumpDir
    ( DumpDir(..)
    , initalState
    , cr
    , jpg
    , unDumpDir
    , File(..)
    )
where

import           Lib.Model.Data
import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

type CR = FilePath
type JPG = FilePath

data File = File
    { _cr :: CR
    , _jpg :: JPG
    }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''File

newtype DumpDir = DumpDir { _unDumpDir :: [File] }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''DumpDir

initalState :: Data String DumpDir
initalState = NotAsked
