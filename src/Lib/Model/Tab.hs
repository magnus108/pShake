{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Tab
    ( Tab(..)
    , Tabs(..)
    , unTabs
    , getTabs
    , writeTabs
    , initalState
    )
where

import           Lib.Model.Data
import           Utils.ListZipper
import           Control.Lens

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

data Tab
    = DumpTab
    | DagsdatoTab
    | DagsdatoBackupTab
    | DoneshootingTab
    | DoneshootingBackupTab
    | PhotographersTab
    | CamerasTab
    | ShootingsTab
    | SessionsTab
    | LocationTab
    | MainTab
    | InsertPhotographeeTab
    | ControlTab
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Tab

newtype Tabs = Tabs { _unTabs :: ListZipper Tab }
    deriving (Eq, Ord, Show)
    deriving (Generic)
    deriving anyclass (FromJSON, ToJSON)

makeLenses ''Tabs


getTabs
    :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> m Tabs
getTabs = readJSONFile

writeTabs
    :: (MonadIO m, MonadThrow m) => FilePath -> Tabs -> m ()
writeTabs = writeJSONFile

initalState :: Data String Tabs
initalState = NotAsked
