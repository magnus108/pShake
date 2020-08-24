module Lib.App.Env
    ( Env(..)
    , mkEnv
    )
where

import           Lib.Config                     ( Config(..) )
import qualified Control.Concurrent.Chan.Unagi as Chan

data Env msg = Env
    { inChan :: Chan.InChan msg
    , outChan :: Chan.OutChan msg
    , mDumpFile :: MVar FilePath
    , mDoneshootingFile :: MVar FilePath
    , mDagsdatoFile :: MVar FilePath
    , mDagsdatoBackupFile :: MVar FilePath
    , mShootingsFile :: MVar FilePath
    , mSessionsFile :: MVar FilePath
    , mPhotographersFile :: MVar FilePath
    , mGradesFile :: MVar FilePath
    , mCamerasFile :: MVar FilePath
    , mTabsFile :: MVar FilePath
    , mLocationConfigFile :: MVar FilePath
    , mTranslationFile :: MVar FilePath
    , mPhotograheesFile :: MVar FilePath
    , mBuildFile :: MVar FilePath
    , port :: Int
    , static :: FilePath
    , index :: FilePath
    }

mkEnv :: MonadIO m => Int -> Config -> m (Env msg)
mkEnv port Config {..} = liftIO $ do
    (inChan, outChan)   <- Chan.newChan
    mPhotographersFile  <- newMVar photographersFile
    mDumpFile           <- newMVar dumpFile
    mDoneshootingFile   <- newMVar doneshootingFile
    mDagsdatoFile       <- newMVar dagsdatoFile
    mDagsdatoBackupFile <- newMVar dagsdatoBackupFile
    mShootingsFile      <- newMVar shootingsFile
    mSessionsFile       <- newMVar sessionsFile
    mGradesFile         <- newMVar gradesFile
    mCamerasFile        <- newMVar camerasFile
    mTabsFile           <- newMVar tabsFile
    mLocationConfigFile <- newMVar locationConfigFile
    mTranslationFile    <- newMVar translationFile
    mPhotograheesFile   <- newMVar photograheesFile
    mBuildFile          <- newMVar buildFile
    let static = "static"
    let index  = "index.html"
    pure Env { .. }
