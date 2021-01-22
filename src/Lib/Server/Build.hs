module Lib.Server.Build
    ( runBuild
    ) where

import System.IO.Error (tryIOError)
import Data.Char
import qualified System.Directory as SD
import qualified Data.List.Index as Indexed
import Development.Shake
import Development.Shake.FilePath
import qualified Development.Shake.FilePath as FP
import qualified Utils.ListZipper              as ListZipper

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


import Data.Time.Format
import Data.Time.Clock



shakeDir :: FilePath
shakeDir = "._build"

getDate :: UTCTime -> String
getDate = formatTime defaultTimeLocale "%Y - %m%d"


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


getOpts :: forall  r m . WithChan r m => m ShakeOptions
getOpts = return $ shakeOptions
        { shakeFiles = shakeDir
        , shakeThreads = 0
        }


readDoneshootingDir :: forall  r m . WithChan r m => m [FilePath]
readDoneshootingDir = do
    grades <- readGrades
    let grade = view (Grade.unGrades . ListZipper.zipperL . Grade.gradeId) grades
    location <- readLocation
    let locationName = takeBaseName $ view Location.unLocation location
    sessions <- readSessions
    let sessionId = Session.toInteger $ sessions ^. Session.unSessions . ListZipper.zipperL
    cameras <- readCameras
    let extension = filter ((/=) '.') $ Camera.toExtension $ cameras ^. Camera.unCameras . ListZipper.zipperL
    photographers <- readPhotographers
    let photographerId = photographers ^. Photographer.unPhotographers . ListZipper.zipperL . Photographer.tid
    shootings <- readShootings
    sessions <- readSessions
    let session = sessions ^. Session.unSessions . ListZipper.zipperL
    let shootingId = show $ if session == Session.KindergartenGroup then 3 else Shooting.toInteger $ shootings ^. Shooting.unShootings . ListZipper.zipperL
    doneshooting <- readDoneshooting
    let doneshootingPath = doneshooting ^. Doneshooting.unDoneshooting
    let path = doneshootingPath </> locationName </> extension </> grade
    files <- liftIO $ tryIOError $ SD.listDirectory path
    return $ either (\_ -> []) (\xs -> xs) files


mkDoneshootingPathJpg :: forall  r m . WithChan r m => m (Int -> FilePath -> FilePath)
mkDoneshootingPathJpg = do
    location <- readLocation
    sessions <- readSessions
    cameras <- readCameras
    photographers <- readPhotographers
    doneshooting <- readDoneshooting
    shootings <- readShootings
    sessions <- readSessions
    grades <- readGrades

    return $ \index file ->
        let locationName = takeBaseName $ view Location.unLocation location
            sessionId = show $ Session.toInteger $ sessions ^. Session.unSessions . ListZipper.zipperL

            extension = filter ((/=) '.') $ Camera.toExtension $ cameras ^. Camera.unCameras . ListZipper.zipperL

            photographerId = photographers ^. Photographer.unPhotographers . ListZipper.zipperL . Photographer.tid

            doneshootingPath = doneshooting ^. Doneshooting.unDoneshooting

            session = sessions ^. Session.unSessions . ListZipper.zipperL
            shootingId = show $ if session == Session.KindergartenGroup then 3 else Shooting.toInteger $ shootings ^. Shooting.unShootings . ListZipper.zipperL

            grade = view (Grade.unGrades . ListZipper.zipperL) grades
            tid = grade ^. Grade.photographees . Grade.unPhotographees . ListZipper.zipperL . Grade.tid

            fileExtension = toLower <$> (FP.takeExtension file)

            pad x = padLeft '0' 3 (show x)
            no = pad index

            path = doneshootingPath </> locationName </> extension </> "_webshop" </> sessionId ++ "." ++ tid ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ fileExtension
        in
            path

mkDoneshootingPath :: forall  r m . WithChan r m => m (Int -> FilePath -> FilePath)
mkDoneshootingPath = do
    location <- readLocation
    sessions <- readSessions
    cameras <- readCameras
    photographers <- readPhotographers
    doneshooting <- readDoneshooting
    shootings <- readShootings
    sessions <- readSessions
    grades <- readGrades

    return $ \index file ->
        let locationName = takeBaseName $ view Location.unLocation location
            sessionId = show $ Session.toInteger $ sessions ^. Session.unSessions . ListZipper.zipperL

            extension = filter ((/=) '.') $ Camera.toExtension $ cameras ^. Camera.unCameras . ListZipper.zipperL

            photographerId = photographers ^. Photographer.unPhotographers . ListZipper.zipperL . Photographer.tid

            doneshootingPath = doneshooting ^. Doneshooting.unDoneshooting

            session = sessions ^. Session.unSessions . ListZipper.zipperL
            shootingId = show $ if session == Session.KindergartenGroup then 3 else Shooting.toInteger $ shootings ^. Shooting.unShootings . ListZipper.zipperL

            grade = view (Grade.unGrades . ListZipper.zipperL) grades
            gradeId = view Grade.gradeId grade
            tid = grade ^. Grade.photographees . Grade.unPhotographees . ListZipper.zipperL . Grade.tid

            fileExtension = toLower <$> (FP.takeExtension file)

            pad x = padLeft '0' 3 (show x)
            no = pad index

            path = doneshootingPath </> locationName </> extension </> gradeId </> sessionId ++ "." ++ tid ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ fileExtension
        in
            path


mkDagsdatoBackupPath :: forall  r m . WithChan r m => m (String -> FilePath -> FilePath)
mkDagsdatoBackupPath = do
    location <- readLocation
    dagsdatoBackup <- readDagsdatoBackup
    grades <- readGrades
    return $ \date file ->
        let 
            locationName = takeBaseName $ view Location.unLocation location
            grade = view (Grade.unGrades . ListZipper.zipperL) grades
            gradeId = view Grade.gradeId grade
            photographee = grade ^. Grade.photographees . Grade.unPhotographees . ListZipper.zipperL
            tid = photographee ^. Grade.tid
            name = photographee ^. Grade.name
            dagsdatoBackupPath = dagsdatoBackup ^. DagsdatoBackup.unDagsdatoBackup
            fileExtension = toLower <$> (FP.takeExtension file)
            path = dagsdatoBackupPath </> date ++ " - " ++ locationName </> gradeId </> (name ++ " - " ++ tid) </> file -<.> fileExtension
        in
            path

mkDagsdatoPath :: forall  r m . WithChan r m => m (String -> FilePath -> FilePath)
mkDagsdatoPath = do
    location <- readLocation
    dagsdato <- readDagsdato
    grades <- readGrades
    return $ \date file ->
        let 
            locationName = takeBaseName $ view Location.unLocation location
            grade = view (Grade.unGrades . ListZipper.zipperL) grades
            gradeId = view Grade.gradeId grade
            photographee = grade ^. Grade.photographees . Grade.unPhotographees . ListZipper.zipperL
            tid = photographee ^. Grade.tid
            name = photographee ^. Grade.name
            dagsdatoPath = dagsdato ^. Dagsdato.unDagsdato
            fileExtension = toLower <$> (FP.takeExtension file)
            path = dagsdatoPath </> date ++ " - " ++ locationName </> gradeId </> (name ++ " - " ++ tid) </> file -<.> fileExtension
        in
            path


padLeft :: Char -> Int -> String -> String
padLeft c n s = let len = length s
                    padLen = max 0 (n - len)
                    padStr = show $ replicate padLen c
                in  concat [padStr, s]


runBuild :: forall  r m . WithChan r m => m ()
runBuild = do
    time <- liftIO getCurrentTime
    let date = getDate time

    grades <- readGrades
    photographers <- readPhotographers
    dagsdatoBackup <- readDagsdatoBackup
    dagsdato <- readDagsdato
    dumpDir <- readDumpDir
    dump <- readDump
    doneshooting <- readDoneshooting


    traceShowM "FUCK"
    doneshootingDir <- readDoneshootingDir -- SKAL MODELLERS
    let doneshootingFileCount = length doneshootingDir
    traceShowM "FUCK2"

    opts <- getOpts

    doneshootingCrFunc <- mkDoneshootingPath
    doneshootingJpgFunc <- mkDoneshootingPathJpg
    dagsdatoFunc <- mkDagsdatoPath
    dagsdatoBackupFunc <- mkDagsdatoBackupPath
    liftIO $ shake opts $ do
        Indexed.iforM_ (dumpDir ^. DumpDir.unDumpDir) $ \ i f -> do

                let crFile = f ^. DumpDir.cr
                let jpgFile = f ^. DumpDir.jpg

                let index' = doneshootingFileCount + i + 1

                let doneshootingCr = doneshootingCrFunc index' crFile
                let doneshootingJpg = doneshootingJpgFunc index' jpgFile

                let dagsdatoCr = dagsdatoFunc date crFile
                let dagsdatoJpg = dagsdatoFunc date jpgFile

                let dagsdatoBackupCr = dagsdatoBackupFunc date crFile
                let dagsdatoBackupJpg = dagsdatoBackupFunc date jpgFile

                want [doneshootingCr, doneshootingJpg, dagsdatoCr, dagsdatoJpg, dagsdatoBackupCr, dagsdatoBackupJpg]

                traceShowM "FUCK"
                doneshootingCr %> copyFile' crFile
                traceShowM "UNFUCK"

                doneshootingJpg %> copyFile' jpgFile

                dagsdatoCr %> copyFile' crFile

                dagsdatoJpg %> copyFile' jpgFile

                dagsdatoBackupCr %> copyFile' crFile

                dagsdatoBackupJpg %> copyFile' jpgFile

                action $ removeFilesAfter (dump ^. Dump.unDump) ["//*.CR3", "//*.JPG", "//*.cr3", "//*.jpg","//*.CR2","//*.cr2"]
