{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Location
    ( Location(..)
    , getLocation
    , writeLocation
    , initalState
    , unLocation
    )
where

import           Lib.Model.Data
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )


newtype Location = Location { _unLocation :: FilePath }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Location

getLocation
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Location
getLocation = readJSONFile

writeLocation
    :: (MonadIO m, MonadThrow m) => FilePath -> Location -> m ()
writeLocation = writeJSONFile

initalState :: Data String Location
initalState = NotAsked
