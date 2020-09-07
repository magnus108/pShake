{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
    , grab
    )
where

import qualified Data.HashMap.Strict           as HashMap
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan
import qualified Reactive.Threepenny           as Reactive
import qualified System.FSNotify               as FS
import qualified Lib.Model                     as Model
import qualified Lib.Message                   as Message


type StopMap = HashMap.HashMap String FS.StopListening
type StartMap m = HashMap.HashMap String (m FS.StopListening)

data Env (m :: Type -> Type) = Env
    { inChan :: InChan
    , outChan :: OutChan

    , mDumpFile :: !(MVar FilePath)
    , mDoneshootingFile :: !(MVar FilePath)
    , mDagsdatoFile :: !(MVar FilePath)
    , mDagsdatoBackupFile :: !(MVar FilePath)
    , mShootingsFile :: !(MVar FilePath)
    , mSessionsFile :: !(MVar FilePath)
    , mPhotographersFile :: MPhotographersFile
    , mGradesFile :: !(MVar FilePath)
    , mCamerasFile :: !(MVar FilePath)
    , mTabsFile :: !(MVar FilePath)
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
    , eDumpDir :: !(Reactive.Event ())
    , hDumpDir :: !(Reactive.Handler ())
    , eConfigDump :: !(Reactive.Event ())
    , hConfigDump :: !(Reactive.Handler ())
    , eTabs :: !(Reactive.Event ())
    , hTab :: !(Reactive.Handler ())
    , ePhotographers :: !(Reactive.Event Model.Photographers)
    , hPhotographers :: HPhotographers
    , eCameras :: !(Reactive.Event ())
    , hCameras :: !(Reactive.Handler ())
    , eShootings :: !(Reactive.Event ())
    , hShootings :: !(Reactive.Handler ())
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

    , watchManager :: WatchManager
    }

newtype MStartMap m = MStartMap { unMStartMap :: MVar (StartMap m) }

newtype MStopMap = MStopMap { unMStopMap ::  MVar (HashMap.HashMap String FS.StopListening) }

newtype InChan = InChan { unInChan:: Chan.InChan Message.Message }
newtype OutChan = OutChan { unOutChan :: Chan.OutChan Message.Message }

newtype MPhotographersFile = MPhotographersFile { unMPhotographersFile :: MVar FilePath }
newtype HPhotographers = HPhotographers { unHPhotographers :: Reactive.Handler Model.Photographers}

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


grab :: forall  field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
