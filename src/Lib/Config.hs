{-# LANGUAGE DeriveAnyClass #-}
module Lib.Config
    ( Config(..)
    , loadConfig
    )
where

import           System.FilePath

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

data Config = Config
    { dumpFile :: !FilePath
    , doneshootingFile :: !FilePath
    , dagsdatoFile :: !FilePath
    , dagsdatoBackupFile :: !FilePath
    , shootingsFile :: !FilePath
    , sessionsFile :: !FilePath
    , photographersFile :: !FilePath
    , gradesFile :: !FilePath
    , camerasFile :: !FilePath
    , tabsFile :: !FilePath
    , locationFile :: !FilePath
    , translationFile :: !FilePath
    , photograheesFile :: !FilePath
    , buildFile :: !FilePath
    } deriving (Generic)
      deriving (FromJSON, ToJSON)
      deriving (Show)

loadConfig :: (MonadCatch m, MonadIO m, MonadThrow m) => FilePath -> FilePath -> m Config
loadConfig root file = do
    let config = root </> file
    readJSONFile config

