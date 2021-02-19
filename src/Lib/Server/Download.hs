module Lib.Server.Download
    ( runDownload
    ) where


import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import System.IO.Error (tryIOError)
import Data.Char
import qualified System.Directory as SD
import qualified Data.List.Index as Indexed
import Development.Shake
import Development.Shake.FilePath
import qualified Development.Shake.FilePath as FP
import qualified Utils.ListZipper              as ListZipper
import Data.Time
import Data.Time.Clock.POSIX
import Data.Int

import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                , view
                                                )

import           Lib.App                        ( WithError
                                                )

import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , catch
                                                , catchIOError
                                                , catchAll
                                                , try
                                                )
import qualified Control.Monad.Except          as E
                                                ( catchError
                                                , throwError
                                                )

import           Lib.App                        ( runApp
                                                , AppEnv
                                                , readGrades
                                                , readShootings
                                                , readDumpDir
                                                , readPhotographers
                                                , readSessions
                                                , readCameras
                                                , readLocation
                                                , readDoneshooting
                                                , readDagsdato
                                                , readDagsdatoBackup
                                                , readDump
                                                , grab
                                                , AppException(..)
                                                , AppError(..)
                                                , WithError
                                                , IError(..)
                                                , Env(..)
                                                , Has(..)
                                                , HPhotographers(..)
                                                , MPhotographersFile(..)
                                                , HGrades(..)
                                                , MGradesFile(..)
                                                , HDumpDir(..)
                                                , HConfigDumpDir(..)
                                                , MDumpFile(..)
                                                , HDoneshootingDir(..)
                                                , MDoneshootingFile(..)
                                                , HDagsdatoDir(..)
                                                , MDagsdatoFile(..)
                                                , HDagsdatoBackupDir(..)
                                                , MDagsdatoBackupFile(..)
                                                , HTabs(..)
                                                , MTabsFile(..)
                                                , HShootings(..)
                                                , MShootingsFile(..)

                                                , HBuild(..)
                                                , MBuildFile(..)

                                                , HSessions(..)
                                                , MSessionsFile(..)
                                                , HLocationFile(..)
                                                , MLocationFile(..)
                                                , HCameras(..)
                                                , MCamerasFile(..)
                                                , MStartMap(..)
                                                , WatchManager(..)
                                                , MStopMap(..)
                                                , OutChan(..)
                                                , InChan(..)
                                                , unMPhotographersFile
                                                , unHPhotographers
                                                , unMGradesFile
                                                , unHGrades
                                                , unMTabsFile
                                                , unHTabs
                                                , unMShootingsFile
                                                , unHShootings
                                                , unMSessionsFile
                                                , unHSessions
                                                , unMCamerasFile
                                                , unHCameras
                                                , unMStopMap
                                                , unMStartMap
                                                , unOutChan
                                                , unInChan
                                                )
import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import qualified Lib.Model.Build               as Build
import qualified Lib.Model.Grade               as Grade
import qualified Lib.Model.Doneshooting        as Doneshooting
import qualified Lib.Model.Camera              as Camera
import qualified Lib.Model.Dump                as Dump
import qualified Lib.Model.DumpDir                as DumpDir
import qualified Lib.Model.Tab                 as Tab
import qualified Lib.Model.Shooting            as Shooting
import qualified Lib.Model.Session             as Session
import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Model.Data                as Data
import qualified Lib.Model.Dagsdato            as Dagsdato
import qualified Lib.Model.DagsdatoBackup      as DagsdatoBackup
import qualified Lib.Model.Location            as Location

import qualified Codec.Archive.Zip as Zip


type WithChan r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has WatchManager r
      , Has HPhotographers r
      , Has MPhotographersFile r
      , Has HTabs r
      , Has MTabsFile r
      , Has HShootings r
      , Has MShootingsFile r
      , Has HGrades r
      , Has MGradesFile r
      , Has HSessions r
      , Has MSessionsFile r
      , Has HCameras r
      , Has MCamerasFile r
      , Has HDumpDir r
      , Has HBuild r
      , Has HConfigDumpDir r
      , Has MDumpFile r
      , Has HLocationFile r
      , Has MLocationFile r
      , Has HDagsdatoDir r
      , Has MDagsdatoFile r
      , Has HDagsdatoBackupDir r
      , Has MDagsdatoBackupFile r
      , Has HDoneshootingDir r
      , Has MDoneshootingFile r
      , Has MBuildFile r
      , Has (MStartMap m) r
      , Has MStopMap r
      , Has OutChan r
      , Has InChan r
      , MonadIO m
      , MonadCatch m
      )

nanosSinceEpoch :: UTCTime -> Integer
nanosSinceEpoch =
    floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

runDownload :: forall  r m . WithChan r m => m ()
runDownload = do
    time <- liftIO getCurrentTime
    let timeEpoch = nanosSinceEpoch time

    let empty = Zip.emptyArchive
    let entry = Zip.toEntry "photographers.json" timeEpoch "aaaa"
    let myArchive1 = Zip.addEntryToArchive entry empty
    liftIO $ B.writeFile "archive.zip" (BL.toStrict (Zip.fromArchive myArchive1))
