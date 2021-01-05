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

    , HTabs(..)
    , MTabsFile(..)
    , HShootings(..)
    , MShootingsFile(..)
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
import qualified Lib.Model.Data                as Data
import qualified Lib.Message                   as Message


type StopMap = HashMap.HashMap String FS.StopListening
type StartMap m = HashMap.HashMap String (m FS.StopListening)

data Env (m :: Type -> Type) = Env
    { inChan :: InChan
    , outChan :: OutChan

    , mDumpFile :: MDumpFile
    , mDoneshootingFile :: !(MVar FilePath)
    , mDagsdatoFile :: !(MVar FilePath)
    , mDagsdatoBackupFile :: !(MVar FilePath)
    , mShootingsFile :: MShootingsFile
    , mSessionsFile :: !(MVar FilePath)
    , mPhotographersFile :: MPhotographersFile
    , mGradesFile :: !(MVar FilePath)
    , mCamerasFile :: !(MVar FilePath)
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

    , eDirDoneshooting :: !(Reactive.Event ())
    , hDirDoneshooting :: !(Reactive.Handler ())
    , eConfigDoneshooting :: !(Reactive.Event ())
    , hConfigDoneshooting :: !(Reactive.Handler ())
    , eDirDagsdato :: !(Reactive.Event ())
    , hDirDagsdato :: !(Reactive.Handler ())
    , eConfigDagsdato :: !(Reactive.Event ())
    , hConfigDagsdato :: !(Reactive.Handler ())
    , eDirDagsdatoBackup :: !(Reactive.Event ())
    , hDirDagsdatoBackup :: !(Reactive.Handler ())
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
    , eCameras :: !(Reactive.Event ())
    , hCameras :: !(Reactive.Handler ())

    , eShootings :: !(Reactive.Event (Data.Data String Shooting.Shootings))
    , hShootings :: HShootings

    , eSessions :: !(Reactive.Event ())
    , hSessions :: !(Reactive.Handler ())
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

    , bDumpDir :: !(Reactive.Behavior (Data.Data String Dump.Dump))

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


newtype MShootingsFile = MShootingsFile { unMShootingsFile :: MVar FilePath }
newtype HShootings = HShootings { unHShootings :: Reactive.Handler (Data.Data String Shooting.Shootings) }

newtype MDumpFile = MDumpFile { unMDumpFile :: MVar FilePath }
newtype HDumpDir = HDumpDir { unHDumpDir :: Reactive.Handler (Data.Data String Dump.Dump) }

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

instance Has MDumpFile              (Env m) where
    obtain = mDumpFile

instance Has HDumpDir             (Env m) where
    obtain = hDumpDir

grab :: forall  field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
