{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Shooting
    ( Shooting(..)
    , Shootings(..)
    , getShootings
    , writeShootings
    , initalState
    , unShootings
    , toInteger
    )
where

import Prelude hiding (toInteger)
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


toInteger :: Shooting -> Integer
toInteger = \case
    Normal -> 1
    ReShoot -> 2


newtype Shootings = Shootings { _unShootings :: ListZipper Shooting }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Shootings

getShootings
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Shootings
getShootings = readJSONFile


writeShootings
    :: (MonadIO m, MonadThrow m) => FilePath -> Shootings -> m ()
writeShootings = writeJSONFile


initalState :: Data String Shootings
initalState = NotAsked
