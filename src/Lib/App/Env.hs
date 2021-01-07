module Lib.App.Env
    ( Env(..)
    , Has(..)
    , WatchManager(..)
    , StartMap
    , StopMap
    , MStopMap(..)
    , MStartMap(..)
    , InChan(..)
    , OutChan(..)
    , HPhotographers(..)
    , MPhotographersFile(..)
    , HDumpDir(..)
    , MDumpFile(..)
    , HDagsdatoDir(..)
    , MDagsdatoFile(..)
    , HDagsdatoBackupDir(..)
    , MDagsdatoBackupFile(..)
    , HDoneshootingDir(..)
    , MDoneshootingFile(..)
    , HTabs(..)
    , MTabsFile(..)
    , HShootings(..)
    , MShootingsFile(..)
    , HSessions(..)
    , MSessionsFile(..)
    , HCameras(..)
    , MCamerasFile(..)
    , grab
    )
where

import qualified Data.HashMap.Strict           as HashMap
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan
import qualified Reactive.Threepenny           as Reactive
import qualified System.FSNotify               as FS
import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Model.Tab                 as Tab
import qualified Lib.Model.Shooting            as Shooting
import qualified Lib.Model.Dump                as Dump
import qualified Lib.Model.Camera              as Camera
import qualified Lib.Model.Dagsdato            as Dagsdato
import qualified Lib.Model.Session             as Session
import qualified Lib.Model.DagsdatoBackup      as DagsdatoBackup
import qualified Lib.Model.Doneshooting        as Doneshooting
import qualified Lib.Model.Data                as Data
import qualified Lib.Message                   as Message


type StopMap = HashMap.HashMap String FS.StopListening
type StartMap m = HashMap.HashMap String (m FS.StopListening)

data Env (m :: Type -> Type) = Env
    { inChan :: InChan
    , outChan :: OutChan

    , mDumpFile :: MDumpFile
    , mDoneshootingFile :: MDoneshootingFile
    , mDagsdatoFile :: MDagsdatoFile
    , mDagsdatoBackupFile :: MDagsdatoBackupFile
    , mShootingsFile :: MShootingsFile
    , mSessionsFile :: MSessionsFile
    , mPhotographersFile :: MPhotographersFile
    , mGradesFile :: !(MVar FilePath)
    , mCamerasFile :: MCamerasFile
    , mTabsFile :: MTabsFile
    , mLocationConfigFile :: !(MVar FilePath)
    , mTranslationFile :: !(MVar FilePath)
    , mPhotograheesFile :: !(MVar FilePath)
    , mBuildFile :: !(MVar FilePath)

    , mStopMap :: MStopMap
    , mStartMap :: MStartMap m

    , port :: !Int
    , static :: !FilePath
    , index :: !FilePath

    , eDoneshootingDir :: !(Reactive.Event (Data.Data String Doneshooting.Doneshooting))
    , hDoneshootingDir :: HDoneshootingDir

    , eConfigDoneshooting :: !(Reactive.Event ())
    , hConfigDoneshooting :: !(Reactive.Handler ())
    , eDagsdatoDir :: !(Reactive.Event (Data.Data String Dagsdato.Dagsdato))
    , hDagsdatoDir :: HDagsdatoDir
    , eConfigDagsdato :: !(Reactive.Event ())
    , hConfigDagsdato :: !(Reactive.Handler ())
    , eDagsdatoBackupDir :: !(Reactive.Event (Data.Data String DagsdatoBackup.DagsdatoBackup))
    , hDagsdatoBackupDir :: HDagsdatoBackupDir
    , eConfigDagsdatoBackup :: !(Reactive.Event ())
    , hConfigDagsdatoBackup :: !(Reactive.Handler ())
    , eDumpDir :: !(Reactive.Event (Data.Data String Dump.Dump))
    , hDumpDir :: HDumpDir
    , eConfigDump :: !(Reactive.Event ())
    , hConfigDump :: !(Reactive.Handler ())
    , eTabs :: !(Reactive.Event (Data.Data String Tab.Tabs))
    , hTabs :: HTabs
    , ePhotographers :: !(Reactive.Event (Data.Data String Photographer.Photographers))
    , hPhotographers :: HPhotographers
    , eCameras :: !(Reactive.Event (Data.Data String Camera.Cameras))
    , hCameras :: HCameras

    , eShootings :: !(Reactive.Event (Data.Data String Shooting.Shootings))
    , hShootings :: HShootings

    , eSessions :: !(Reactive.Event (Data.Data String Session.Sessions))
    , hSessions :: HSessions
    , eGrades :: !(Reactive.Event ())
    , hGrades :: !(Reactive.Handler ())
    , eLocationConfigFile :: !(Reactive.Event ())
    , hLocationConfigFile :: !(Reactive.Handler ())
    , ePhotographees :: !(Reactive.Event ())
    , hPhotographees :: !(Reactive.Handler ())
    , eBuild :: !(Reactive.Event ())
    , hBuild :: !(Reactive.Handler ())

    , bPhotographers :: !(Reactive.Behavior (Data.Data String Photographer.Photographers))

    , bTabs :: !(Reactive.Behavior (Data.Data String Tab.Tabs))

    , bShootings :: !(Reactive.Behavior (Data.Data String Shooting.Shootings))
    , bSessions :: !(Reactive.Behavior (Data.Data String Session.Sessions))

    , bCameras :: !(Reactive.Behavior (Data.Data String Camera.Cameras))

    , bDumpDir :: !(Reactive.Behavior (Data.Data String Dump.Dump))

    , bDagsdatoDir :: !(Reactive.Behavior (Data.Data String Dagsdato.Dagsdato))

    , bDagsdatoBackupDir :: !(Reactive.Behavior (Data.Data String DagsdatoBackup.DagsdatoBackup))

    , bDoneshootingDir :: !(Reactive.Behavior (Data.Data String Doneshooting.Doneshooting))

    , watchManager :: WatchManager
    }

newtype MStartMap m = MStartMap { unMStartMap :: MVar (StartMap m) }

newtype MStopMap = MStopMap { unMStopMap ::  MVar (HashMap.HashMap String FS.StopListening) }

newtype InChan = InChan { unInChan:: Chan.InChan Message.Message }
newtype OutChan = OutChan { unOutChan :: Chan.OutChan Message.Message }

newtype MPhotographersFile = MPhotographersFile { unMPhotographersFile :: MVar FilePath }
newtype HPhotographers = HPhotographers { unHPhotographers :: Reactive.Handler (Data.Data String Photographer.Photographers) }

newtype MTabsFile = MTabsFile { unMTabsFile :: MVar FilePath }
newtype HTabs = HTabs { unHTabs :: Reactive.Handler (Data.Data String Tab.Tabs) }

newtype MSessionsFile = MSessionsFile { unMSessionsFile :: MVar FilePath }
newtype HSessions = HSessions { unHSessions :: Reactive.Handler (Data.Data String Session.Sessions) }

newtype MShootingsFile = MShootingsFile { unMShootingsFile :: MVar FilePath }
newtype HShootings = HShootings { unHShootings :: Reactive.Handler (Data.Data String Shooting.Shootings) }

newtype MCamerasFile = MCamerasFile { unMCamerasFile :: MVar FilePath }
newtype HCameras = HCameras { unHCameras :: Reactive.Handler (Data.Data String Camera.Cameras) }

newtype MDumpFile = MDumpFile { unMDumpFile :: MVar FilePath }
newtype HDumpDir = HDumpDir { unHDumpDir :: Reactive.Handler (Data.Data String Dump.Dump) }

newtype MDagsdatoFile = MDagsdatoFile { unMDagsdatoFile :: MVar FilePath }
newtype HDagsdatoDir = HDagsdatoDir { unHDagsdatoDir :: Reactive.Handler (Data.Data String Dagsdato.Dagsdato) }

newtype MDagsdatoBackupFile = MDagsdatoBackupFile { unMDagsdatoBackupFile :: MVar FilePath }
newtype HDagsdatoBackupDir = HDagsdatoBackupDir { unHDagsdatoBackupDir :: Reactive.Handler (Data.Data String DagsdatoBackup.DagsdatoBackup) }

newtype MDoneshootingFile = MDoneshootingFile { unMDoneshootingFile :: MVar FilePath }
newtype HDoneshootingDir = HDoneshootingDir { unHDoneshootingDir :: Reactive.Handler (Data.Data String Doneshooting.Doneshooting) }

newtype WatchManager = WatchManager { unWatchManager :: FS.WatchManager }


class Has field env where
    obtain :: env -> field

instance Has WatchManager               (Env m) where
    obtain = watchManager

instance Has (MStartMap m)               (Env m) where
    obtain = mStartMap

instance Has MStopMap                (Env m) where
    obtain = mStopMap

instance Has InChan              (Env m) where
    obtain = inChan

instance Has OutChan              (Env m) where
    obtain = outChan

instance Has MPhotographersFile              (Env m) where
    obtain = mPhotographersFile

instance Has HPhotographers             (Env m) where
    obtain = hPhotographers

instance Has MTabsFile              (Env m) where
    obtain = mTabsFile

instance Has HTabs             (Env m) where
    obtain = hTabs

instance Has MShootingsFile              (Env m) where
    obtain = mShootingsFile

instance Has HShootings             (Env m) where
    obtain = hShootings

instance Has MSessionsFile              (Env m) where
    obtain = mSessionsFile

instance Has HSessions             (Env m) where
    obtain = hSessions

instance Has MCamerasFile              (Env m) where
    obtain = mCamerasFile

instance Has HCameras             (Env m) where
    obtain = hCameras

instance Has MDumpFile              (Env m) where
    obtain = mDumpFile

instance Has HDumpDir             (Env m) where
    obtain = hDumpDir

instance Has MDagsdatoFile              (Env m) where
    obtain = mDagsdatoFile

instance Has HDagsdatoDir             (Env m) where
    obtain = hDagsdatoDir

instance Has MDagsdatoBackupFile              (Env m) where
    obtain = mDagsdatoBackupFile

instance Has HDagsdatoBackupDir             (Env m) where
    obtain = hDagsdatoBackupDir

instance Has MDoneshootingFile              (Env m) where
    obtain = mDoneshootingFile

instance Has HDoneshootingDir             (Env m) where
    obtain = hDoneshootingDir

grab :: forall  field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
