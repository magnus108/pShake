{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Build
    ( Build(..)
    , getBuild
    , writeBuild
    , initalState
    )
where

import qualified Lib.Model.Grade as Grade


import           Lib.Model.Data
import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

type Meta = String

data Build
    = DoneBuild Grade.Photographee Meta
    | Building Grade.Photographee Meta
    | NoBuild
    | NoJpgBuild
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Build


getBuild
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Build
getBuild = readJSONFile

writeBuild
    :: (MonadIO m, MonadThrow m) => FilePath -> Build -> m ()
writeBuild = writeJSONFile

initalState :: Data String Build
initalState = NotAsked
