module Lib.App.Env
    ( Env(..)
    , Has(..)
    , readGrades
    , readDagsdatoBackup
    , readCameras
    , readDumpDir
    , readDoneshooting
    , readShootings
    , readDump
    , readSessions
    , readPhotographers
    , readDagsdato
    , readLocation
    , readDagsdatoBackup
    , WatchManager(..)
    , StartMap
    , StopMap
    , MStopMap(..)
    , MStartMap(..)
    , InChan(..)
    , OutChan(..)
    , HPhotographers(..)
    , MPhotographersFile(..)

    , HGrades(..)
    , MGradesFile(..)

    , HBuild(..)
    , MBuildFile(..)

    , HLocationFile(..)
    , MLocationFile(..)

    , HConfigDumpDir(..)

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

import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                )
import qualified Control.Lens                  as Lens

import Data.Char
import Data.List
import qualified System.FilePath as FP
import System.Directory

import           Control.Monad.Except           ( MonadError )

import           Lib.App.Error                       ( AppException(..)
                                                , AppError(..)
                                                     , IError(..)
                                                     , WithError
                                                )
import qualified Utils.ListZipper              as ListZipper
import qualified Control.Monad.Except          as E
                                                ( catchError
                                                , throwError
                                                )

import           System.IO.Error                ( IOError
                                                , isUserError
                                                )

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , catch
                                                , catchIOError
                                                , catchAll
                                                , try
                                                )

import qualified Data.HashMap.Strict           as HashMap
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan
import qualified Reactive.Threepenny           as Reactive
import qualified System.FSNotify               as FS
import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Model.Tab                 as Tab
import qualified Lib.Model.Shooting            as Shooting
import qualified Lib.Model.Dump                as Dump
import qualified Lib.Model.Build                as Build
import qualified Lib.Model.DumpDir                as DumpDir
import qualified Lib.Model.Camera              as Camera
import qualified Lib.Model.Dagsdato            as Dagsdato
import qualified Lib.Model.Session             as Session
import qualified Lib.Model.DagsdatoBackup      as DagsdatoBackup
import qualified Lib.Model.Doneshooting        as Doneshooting
import qualified Lib.Model.Grade        as Grade
import qualified Lib.Model.Location        as Location
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
    , mGradesFile :: MGradesFile
    , mCamerasFile :: MCamerasFile
    , mTabsFile :: MTabsFile
    , mLocationFile :: MLocationFile
    , mTranslationFile :: !(MVar FilePath)
    , mBuildFile :: MBuildFile

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
    , eConfigDumpDir :: !(Reactive.Event (Data.Data String DumpDir.DumpDir))
    , hConfigDumpDir :: HConfigDumpDir


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
    , eGrades :: !(Reactive.Event (Data.Data String Grade.Grades))
    , hGrades :: HGrades
    , eLocationFile :: !(Reactive.Event (Data.Data String Location.Location))
    , hLocationFile :: HLocationFile
    , ePhotographees :: !(Reactive.Event ())
    , hPhotographees :: !(Reactive.Handler ())
    , eBuild :: !(Reactive.Event (Data.Data String Build.Build))
    , hBuild :: HBuild

    , bPhotographers :: !(Reactive.Behavior (Data.Data String Photographer.Photographers))

    , bTabs :: !(Reactive.Behavior (Data.Data String Tab.Tabs))

    , bShootings :: !(Reactive.Behavior (Data.Data String Shooting.Shootings))
    , bBuild :: !(Reactive.Behavior (Data.Data String Build.Build))
    , bSessions :: !(Reactive.Behavior (Data.Data String Session.Sessions))

    , bCameras :: !(Reactive.Behavior (Data.Data String Camera.Cameras))

    , bDumpDir :: !(Reactive.Behavior (Data.Data String Dump.Dump))
    , bConfigDumpDir :: !(Reactive.Behavior (Data.Data String DumpDir.DumpDir))

    , bDagsdatoDir :: !(Reactive.Behavior (Data.Data String Dagsdato.Dagsdato))

    , bLocationFile :: !(Reactive.Behavior (Data.Data String Location.Location))
    , bGrades :: !(Reactive.Behavior (Data.Data String Grade.Grades))

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

newtype MGradesFile = MGradesFile { unMGradesFile :: MVar FilePath }
newtype HGrades = HGrades { unHGrades :: Reactive.Handler (Data.Data String Grade.Grades) }

newtype MShootingsFile = MShootingsFile { unMShootingsFile :: MVar FilePath }
newtype HShootings = HShootings { unHShootings :: Reactive.Handler (Data.Data String Shooting.Shootings) }

newtype MCamerasFile = MCamerasFile { unMCamerasFile :: MVar FilePath }
newtype HCameras = HCameras { unHCameras :: Reactive.Handler (Data.Data String Camera.Cameras) }

newtype MDumpFile = MDumpFile { unMDumpFile :: MVar FilePath }
newtype HDumpDir = HDumpDir { unHDumpDir :: Reactive.Handler (Data.Data String Dump.Dump) }
newtype HConfigDumpDir = HConfigDumpDir { unHConfigDumpDir :: Reactive.Handler (Data.Data String DumpDir.DumpDir) }

newtype MLocationFile = MLocationFile { unMLocationFile :: MVar FilePath }
newtype HLocationFile = HLocationFile { unHLocationFile :: Reactive.Handler (Data.Data String Location.Location) }

newtype MDagsdatoFile = MDagsdatoFile { unMDagsdatoFile :: MVar FilePath }
newtype HDagsdatoDir = HDagsdatoDir { unHDagsdatoDir :: Reactive.Handler (Data.Data String Dagsdato.Dagsdato) }

newtype MDagsdatoBackupFile = MDagsdatoBackupFile { unMDagsdatoBackupFile :: MVar FilePath }
newtype HDagsdatoBackupDir = HDagsdatoBackupDir { unHDagsdatoBackupDir :: Reactive.Handler (Data.Data String DagsdatoBackup.DagsdatoBackup) }

newtype MDoneshootingFile = MDoneshootingFile { unMDoneshootingFile :: MVar FilePath }
newtype HDoneshootingDir = HDoneshootingDir { unHDoneshootingDir :: Reactive.Handler (Data.Data String Doneshooting.Doneshooting) }

newtype MBuildFile = MBuildFile { unMBuildFile :: MVar FilePath }
newtype HBuild = HBuild { unHBuild :: Reactive.Handler (Data.Data String Build.Build) }

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

instance Has MGradesFile              (Env m) where
    obtain = mGradesFile

instance Has HGrades             (Env m) where
    obtain = hGrades

instance Has MBuildFile              (Env m) where
    obtain = mBuildFile

instance Has HBuild             (Env m) where
    obtain = hBuild

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

instance Has HConfigDumpDir             (Env m) where
    obtain = hConfigDumpDir

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

instance Has MLocationFile              (Env m) where
    obtain = mLocationFile

instance Has HLocationFile             (Env m) where
    obtain = hLocationFile

grab :: forall  field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field




type WithGrades r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has HGrades r
      , Has MGradesFile r
      , MonadIO m
      , MonadCatch m
      , WithError m
      )


readGrades :: forall  r m . WithGrades r m => m Grade.Grades
readGrades = do
    mGradesFile <- unMGradesFile <$> grab @MGradesFile
    gradesFile  <- takeMVar mGradesFile
    grades <- readJSONFile gradesFile
        `catchIOError` (\e -> do
                           putMVar mGradesFile gradesFile
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
    putMVar mGradesFile gradesFile
    return grades


type WithPhotographers r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has HPhotographers r
      , Has MPhotographersFile r
      , MonadIO m
      , MonadCatch m
      , WithError m
      )

readPhotographers :: forall  r m . WithPhotographers r m => m Photographer.Photographers
readPhotographers = do
    mPhotographersFile <- unMPhotographersFile <$> grab @MPhotographersFile
    photographersFile  <- takeMVar mPhotographersFile
    photographers <- readJSONFile photographersFile
        `catchIOError` (\e -> do
                           putMVar mPhotographersFile photographersFile
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
    putMVar mPhotographersFile photographersFile
    return photographers



type WithDump r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has MDumpFile r
      , MonadIO m
      , MonadCatch m
      , WithError m
      )

readDump :: forall  r m . WithDump r m => m Dump.Dump
readDump = do
    mDumpFile <- unMDumpFile <$> grab @MDumpFile
    dumpFile  <- takeMVar mDumpFile
    dump <- readJSONFile dumpFile
        `catchIOError` (\e -> do
                           putMVar mDumpFile dumpFile
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
    putMVar mDumpFile dumpFile
    return dump


type WithDoneshooting r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has HDoneshootingDir r
      , Has MDoneshootingFile r
      , MonadIO m
      , MonadCatch m
      , WithError m
      )

readDoneshooting :: forall  r m . WithDoneshooting r m => m Doneshooting.Doneshooting
readDoneshooting = do
    mDoneshootingFile <- unMDoneshootingFile <$> grab @MDoneshootingFile
    doneshootingFile  <- takeMVar mDoneshootingFile
    doneshooting <- readJSONFile doneshootingFile
        `catchIOError` (\e -> do
                           putMVar mDoneshootingFile doneshootingFile
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
    putMVar mDoneshootingFile doneshootingFile
    return doneshooting



type WithDagsdato r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has HDagsdatoDir r
      , Has MDagsdatoFile r
      , MonadIO m
      , MonadCatch m
      , WithError m
      )

readDagsdato :: forall  r m . WithDagsdato r m => m Dagsdato.Dagsdato
readDagsdato = do
    mDagsdatoFile <- unMDagsdatoFile <$> grab @MDagsdatoFile
    dagsdatoFile  <- takeMVar mDagsdatoFile
    dagsdato <- readJSONFile dagsdatoFile
        `catchIOError` (\e -> do
                           putMVar mDagsdatoFile dagsdatoFile
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
    putMVar mDagsdatoFile dagsdatoFile
    return dagsdato


type WithDagsdatoBackup r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has HDagsdatoBackupDir r
      , Has MDagsdatoBackupFile r
      , MonadIO m
      , MonadCatch m
      , WithError m
      )

readDagsdatoBackup :: forall  r m . WithDagsdatoBackup r m => m DagsdatoBackup.DagsdatoBackup
readDagsdatoBackup = do
    mDagsdatoBackupFile <- unMDagsdatoBackupFile <$> grab @MDagsdatoBackupFile
    dagsdatoBackupFile  <- takeMVar mDagsdatoBackupFile
    dagsdatoBackup <- readJSONFile dagsdatoBackupFile
        `catchIOError` (\e -> do
                           putMVar mDagsdatoBackupFile dagsdatoBackupFile
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
    putMVar mDagsdatoBackupFile dagsdatoBackupFile
    return dagsdatoBackup


type WithLocation r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has HLocationFile r
      , Has MLocationFile r
      , MonadIO m
      , MonadCatch m
      , WithError m
      )

readLocation :: forall  r m . WithLocation r m => m Location.Location
readLocation = do
    mLocationFile <- unMLocationFile <$> grab @MLocationFile
    locationFile  <- takeMVar mLocationFile
    location <- readJSONFile locationFile
        `catchIOError` (\e -> do
                           putMVar mLocationFile locationFile
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
    putMVar mLocationFile locationFile
    return location


type WithSessions r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has HSessions r
      , Has MSessionsFile r
      , MonadIO m
      , MonadCatch m
      , WithError m
      )

readSessions :: forall  r m . WithSessions r m => m Session.Sessions
readSessions = do
    mSessionsFile <- unMSessionsFile <$> grab @MSessionsFile
    sessionsFile <- takeMVar mSessionsFile
    sessions <- readJSONFile sessionsFile
        `catchIOError` (\e -> do
                           putMVar mSessionsFile sessionsFile
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
    putMVar mSessionsFile sessionsFile
    return sessions


type WithCameras r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has MCamerasFile r
      , MonadIO m
      , MonadCatch m
      , WithError m
      )

readCameras :: forall  r m . WithCameras r m => m Camera.Cameras
readCameras = do
    mCamerasFile <- unMCamerasFile <$> grab @MCamerasFile
    camerasFile <- takeMVar mCamerasFile
    cameras <- readJSONFile camerasFile
        `catchIOError` (\e -> do
                           putMVar mCamerasFile camerasFile
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
    putMVar mCamerasFile camerasFile
    return cameras


type WithShootings r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has MShootingsFile r
      , MonadIO m
      , MonadCatch m
      , WithError m
      )

readShootings :: forall  r m . WithShootings r m => m Shooting.Shootings
readShootings = do
    mShootingsFile <- unMShootingsFile <$> grab @MShootingsFile
    shootingsFile <- takeMVar mShootingsFile
    shootings <- readJSONFile shootingsFile
        `catchIOError` (\e -> do
                           putMVar mShootingsFile shootingsFile
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
    putMVar mShootingsFile shootingsFile
    return shootings



type WithDumpDir r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has MCamerasFile r
      , Has MDumpFile r
      , MonadIO m
      , MonadCatch m
      , WithError m
      )

readDumpDir :: forall r m . WithDumpDir r m => m DumpDir.DumpDir
readDumpDir = do {
    dump <- readDump
    ;cameras <- readCameras
    ;let extension = Camera.toExtension $ cameras ^. Camera.unCameras . ListZipper.zipperL
    ;let filepath = Lens.view Dump.unDump dump

    ;files <- liftIO $ listDirectory filepath

    ;let (crs, jpgs) = partition (\x -> FP.takeExtension x == extension ) $
            filter (\x -> 
                        let
                        ext = fmap toLower (FP.takeExtension x)
                    in 
                        (trace ext ext) == extension || ext == ".jpg"
                   ) 
            (sort files)

    ;let pairUp = catMaybes [if FP.dropExtension i == (FP.dropExtension j) then Just (i,j) else Nothing | i <- crs, j <- jpgs]

    ;traceShowM pairUp
    -- Pair them
    ;let pairs = DumpDir.DumpDir (fmap (\(x,y) -> DumpDir.File x y) pairUp)
    ;return pairs
    } 

