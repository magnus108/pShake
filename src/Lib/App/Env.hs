module Lib.App.Env
    ( Env(..)
    , mkEnv
    , StartMap
    , StopMap
    )
where

import qualified Data.HashMap.Strict           as HashMap
import           Lib.Config                     ( Config(..) )
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan
import qualified Reactive.Threepenny           as Reactive
import qualified System.FSNotify               as FS
import qualified Data.Time.Clock               as Clock
import qualified Lib.Model                     as Model
import qualified Lib.Message                   as Message


type StopMap = HashMap.HashMap String FS.StopListening
type StartMap m = HashMap.HashMap String (Env m Message.Message -> m FS.StopListening)

data Env m msg = Env
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

    , mStopMap :: MVar StopMap
    , mStartMap :: MVar (StartMap m)

    , port :: Int
    , static :: FilePath
    , index :: FilePath

    , eDirDoneshooting :: Reactive.Event ()
    , hDirDoneshooting :: Reactive.Handler ()
    , eConfigDoneshooting :: Reactive.Event ()
    , hConfigDoneshooting :: Reactive.Handler ()
    , eDirDagsdato :: Reactive.Event ()
    , hDirDagsdato :: Reactive.Handler ()
    , eConfigDagsdato :: Reactive.Event ()
    , hConfigDagsdato :: Reactive.Handler ()
    , eDirDagsdatoBackup :: Reactive.Event ()
    , hDirDagsdatoBackup :: Reactive.Handler ()
    , eConfigDagsdatoBackup :: Reactive.Event ()
    , hConfigDagsdatoBackup :: Reactive.Handler ()
    , eDumpDir :: Reactive.Event ()
    , hDumpDir :: Reactive.Handler ()
    , eConfigDump :: Reactive.Event ()
    , hConfigDump :: Reactive.Handler ()
    , eTabs :: Reactive.Event ()
    , hTab :: Reactive.Handler ()
    , ePhotographers :: Reactive.Event ()
    , hPhotographers :: Reactive.Handler ()
    , eCameras :: Reactive.Event ()
    , hCameras :: Reactive.Handler ()
    , eShootings :: Reactive.Event ()
    , hShootings :: Reactive.Handler ()
    , eSessions :: Reactive.Event ()
    , hSessions :: Reactive.Handler ()
    , eGrades :: Reactive.Event ()
    , hGrades :: Reactive.Handler ()
    , eLocationConfigFile :: Reactive.Event ()
    , hLocationConfigFile :: Reactive.Handler ()
    , ePhotographees :: Reactive.Event Model.Photographers
    , hPhotographees :: Reactive.Handler Model.Photographers
    , eBuild :: Reactive.Event ()
    , hBuild :: Reactive.Handler ()

    , watchManager :: FS.WatchManager
    }

mkEnv :: MonadIO m => Int -> Config -> m (Env m msg)
mkEnv port Config {..} = liftIO $ do
    (inChan, outChan)   <- Chan.newChan 200
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

    mStopMap            <- newMVar mempty
    mStartMap           <- newMVar mempty

    let static = "static"
    let index  = "index.html"

    (eDirDoneshooting     , hDirDoneshooting     ) <- Reactive.newEvent
    (eConfigDoneshooting  , hConfigDoneshooting  ) <- Reactive.newEvent
    (eDirDagsdato         , hDirDagsdato         ) <- Reactive.newEvent
    (eConfigDagsdato      , hConfigDagsdato      ) <- Reactive.newEvent
    (eDirDagsdatoBackup   , hDirDagsdatoBackup   ) <- Reactive.newEvent
    (eConfigDagsdatoBackup, hConfigDagsdatoBackup) <- Reactive.newEvent
    (eDumpDir             , hDumpDir             ) <- Reactive.newEvent
    (eConfigDump          , hConfigDump          ) <- Reactive.newEvent
    (eTabs                , hTab                 ) <- Reactive.newEvent
    (ePhotographers       , hPhotographers       ) <- Reactive.newEvent
    (eCameras             , hCameras             ) <- Reactive.newEvent
    (eShootings           , hShootings           ) <- Reactive.newEvent
    (eSessions            , hSessions            ) <- Reactive.newEvent
    (eGrades              , hGrades              ) <- Reactive.newEvent
    (eLocationConfigFile  , hLocationConfigFile  ) <- Reactive.newEvent
    (ePhotographees       , hPhotographees       ) <- Reactive.newEvent
    (eBuild               , hBuild               ) <- Reactive.newEvent

    watchManager <- FS.startManagerConf
        (FS.defaultConfig
            { FS.confDebounce = FS.Debounce (Clock.secondsToNominalDiffTime 0.2)
            }
        )

    pure Env { .. }
