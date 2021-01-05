{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Shooting
    ( Shooting(..)
    , Shootings(..)
    , getShootings
    , writeShootings
    , initalState
    )
where

import           Lib.Model.Data
import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

data Shooting
    = ReShoot
    | Normal
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Shooting


newtype Shootings = Shootings { unShootings :: ListZipper Shooting }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


getShootings
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Shootings
getShootings = readJSONFile

writeShootings
    :: (MonadIO m, MonadThrow m) => FilePath -> Shootings -> m ()
writeShootings = writeJSONFile

initalState :: Data String Shootings
initalState = NotAsked
