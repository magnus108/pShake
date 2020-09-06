module Lib
    ( main
    )
where

import qualified Data.HashMap.Strict           as HashMap
import qualified Lib.Message                   as Message
import qualified Lib.Watchers                  as Watchers
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import           Lib.App                        ( AppEnv
                                                , Env(..)
                                                )
import qualified Lib.App                       as App
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import qualified Lib.Model.Photographer        as Photographer
import qualified Data.HashMap.Strict           as HashMap
import qualified Lib.Config                    as Config

import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan
import qualified Reactive.Threepenny           as Reactive
import qualified System.FSNotify               as FS
import qualified Data.Time.Clock               as Clock
import qualified Lib.Model                     as Model
import qualified Lib.Message                   as Message


import           Lib.Config                     ( loadConfig )
import           Lib.Server                     ( setup )


mkAppEnv :: Int -> Config.Config -> IO AppEnv
mkAppEnv port Config.Config {..} = do
    (inChan', outChan') <- Chan.newChan 200
    let inChan  = App.InChan inChan'
    let outChan = App.OutChan outChan'

    mPhotographersFile' <- newMVar photographersFile
    let mPhotographersFile = App.MPhotographersFile mPhotographersFile'
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

    mStopMap'           <- newMVar mempty
    let mStopMap = App.MStopMap mStopMap'
    mStartMap' <- newMVar mempty
    let mStartMap = App.MStartMap mStartMap'

    let static    = "static"
    let index     = "index.html"

    (eDirDoneshooting     , hDirDoneshooting     ) <- Reactive.newEvent
    (eConfigDoneshooting  , hConfigDoneshooting  ) <- Reactive.newEvent
    (eDirDagsdato         , hDirDagsdato         ) <- Reactive.newEvent
    (eConfigDagsdato      , hConfigDagsdato      ) <- Reactive.newEvent
    (eDirDagsdatoBackup   , hDirDagsdatoBackup   ) <- Reactive.newEvent
    (eConfigDagsdatoBackup, hConfigDagsdatoBackup) <- Reactive.newEvent
    (eDumpDir             , hDumpDir             ) <- Reactive.newEvent
    (eConfigDump          , hConfigDump          ) <- Reactive.newEvent
    (eTabs                , hTab                 ) <- Reactive.newEvent
    (ePhotographers       , hPhotographers'      ) <- Reactive.newEvent
    let hPhotographers = App.HPhotographers hPhotographers'
    (eCameras           , hCameras           ) <- Reactive.newEvent
    (eShootings         , hShootings         ) <- Reactive.newEvent
    (eSessions          , hSessions          ) <- Reactive.newEvent
    (eGrades            , hGrades            ) <- Reactive.newEvent
    (eLocationConfigFile, hLocationConfigFile) <- Reactive.newEvent
    (ePhotographees     , hPhotographees     ) <- Reactive.newEvent
    (eBuild             , hBuild             ) <- Reactive.newEvent

    watchManager                               <- FS.startManagerConf
        (FS.defaultConfig
            { FS.confDebounce = FS.Debounce (Clock.secondsToNominalDiffTime 0.2)
            }
        )

    pure Env { .. }


runServer :: AppEnv -> IO ()
runServer env@Env {..} = do
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           }
        $ setup env


main :: Int -> FilePath -> IO ()
main port root = do
    loadConfig root "config.json" >>= mkAppEnv port >>= runServer
