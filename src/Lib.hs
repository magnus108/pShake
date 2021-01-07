module Lib
    ( main
    )
where

import           Lib.App                        ( AppEnv
                                                , Env(..)
                                                )
import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Model.Tab                 as Tab

import qualified Lib.Model.Shooting            as Shooting

import qualified Lib.Model.Dump                as Dump
import qualified Lib.Model.Dagsdato                as Dagsdato
import qualified Lib.Model.Camera                as Camera

import qualified Lib.App                       as App
import           Graphics.UI.Threepenny.Core
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import qualified Lib.Config                    as Config

import qualified Reactive.Threepenny           as Reactive
import qualified System.FSNotify               as FS
import qualified Data.Time.Clock               as Clock


import           Lib.Config                     ( loadConfig )
import           Lib.Server                     ( setup )


mkAppEnv :: Int -> Config.Config -> IO AppEnv
mkAppEnv port Config.Config {..} = do
    (inChan', outChan') <- Chan.newChan 200
    let inChan  = App.InChan inChan'
    let outChan = App.OutChan outChan'

    mPhotographersFile' <- newMVar photographersFile
    let mPhotographersFile = App.MPhotographersFile mPhotographersFile'
    mDumpFile' <- newMVar dumpFile
    let mDumpFile = App.MDumpFile mDumpFile'

    mDoneshootingFile   <- newMVar doneshootingFile
    mDagsdatoFile' <- newMVar dagsdatoFile
    let mDagsdatoFile = App.MDagsdatoFile mDagsdatoFile'

    mDagsdatoBackupFile <- newMVar dagsdatoBackupFile

    mShootingsFile'     <- newMVar shootingsFile
    let mShootingsFile = App.MShootingsFile mShootingsFile'

    mSessionsFile <- newMVar sessionsFile
    mGradesFile   <- newMVar gradesFile

    mCamerasFile' <- newMVar camerasFile
    let mCamerasFile = App.MCamerasFile mCamerasFile'

    mTabsFile'    <- newMVar tabsFile
    let mTabsFile = App.MTabsFile mTabsFile'

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
    (eDagsdatoDir , hDagsdatoDir' ) <- Reactive.newEvent
    let hDagsdatoDir = App.HDagsdatoDir hDagsdatoDir'

    (eConfigDagsdato      , hConfigDagsdato      ) <- Reactive.newEvent
    (eDirDagsdatoBackup   , hDirDagsdatoBackup   ) <- Reactive.newEvent
    (eConfigDagsdatoBackup, hConfigDagsdatoBackup) <- Reactive.newEvent

    (eDumpDir             , hDumpDir'             ) <- Reactive.newEvent
    let hDumpDir = App.HDumpDir hDumpDir'

    (eConfigDump          , hConfigDump          ) <- Reactive.newEvent

    (eTabs                , hTabs'               ) <- Reactive.newEvent
    let hTabs = App.HTabs hTabs'

    (ePhotographers, hPhotographers') <- Reactive.newEvent
    let hPhotographers = App.HPhotographers hPhotographers'
    (eCameras  , hCameras' ) <- Reactive.newEvent
    let hCameras = App.HCameras hCameras'

    (eShootings, hShootings') <- Reactive.newEvent
    let hShootings = App.HShootings hShootings'

    (eSessions          , hSessions          ) <- Reactive.newEvent
    (eGrades            , hGrades            ) <- Reactive.newEvent
    (eLocationConfigFile, hLocationConfigFile) <- Reactive.newEvent
    (ePhotographees     , hPhotographees     ) <- Reactive.newEvent
    (eBuild             , hBuild             ) <- Reactive.newEvent

    watchManager'                              <- FS.startManagerConf
        (FS.defaultConfig
            { FS.confDebounce = FS.Debounce
                                    (Clock.secondsToNominalDiffTime 0.0000001)
            }
        )
    let watchManager = App.WatchManager watchManager'

    bPhotographers <- Reactive.stepper Photographer.initalState ePhotographers

    bTabs          <- Reactive.stepper Tab.initalState eTabs

    bShootings     <- Reactive.stepper Shooting.initalState eShootings

    bCameras     <- Reactive.stepper Camera.initalState eCameras

    bDumpDir     <- Reactive.stepper Dump.initalState eDumpDir

    bDagsdatoDir <- Reactive.stepper Dagsdato.initalState eDagsdatoDir

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
