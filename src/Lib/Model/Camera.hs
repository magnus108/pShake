{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Camera
    ( Camera(..)
    , Cameras(..)
    , getCameras
    , writeCameras
    , initalState
    , unCameras
    , toExtension
    )
where

import           Lib.Model.Data
import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

data Camera
    = CR2
    | CR3
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Camera


newtype Cameras = Cameras { _unCameras :: ListZipper Camera }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)


makeLenses ''Cameras


toExtension :: Camera -> String
toExtension CR2 = ".cr2"
toExtension CR3 = ".cr3"


getCameras
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Cameras
getCameras = readJSONFile

writeCameras
    :: (MonadIO m, MonadThrow m) => FilePath -> Cameras -> m ()
writeCameras = writeJSONFile

initalState :: Data String Cameras
initalState = NotAsked
