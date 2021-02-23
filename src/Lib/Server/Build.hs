module Lib.Server.Build
    ( runBuild
    )
where

import           System.IO.Error                ( tryIOError )
import           Data.Char
import qualified System.Directory              as SD
import qualified Data.List.Index               as Indexed
import           Development.Shake
import           Development.Shake.FilePath
import qualified Development.Shake.FilePath    as FP
import qualified Utils.ListZipper              as ListZipper

import           Control.Lens                   ( view
                                                , (^.)
                                                )


import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

import qualified Lib.App                       as App
import qualified Lib.Model.Grade               as Grade
import qualified Lib.Model.Doneshooting        as Doneshooting
import qualified Lib.Model.Camera              as Camera
import qualified Lib.Model.Dump                as Dump
import qualified Lib.Model.DumpDir             as DumpDir
import qualified Lib.Model.Shooting            as Shooting
import qualified Lib.Model.Session             as Session
import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Model.Dagsdato            as Dagsdato
import qualified Lib.Model.DagsdatoBackup      as DagsdatoBackup
import qualified Lib.Model.Location            as Location


import           Data.Time.Format
import           Data.Time.Clock



shakeDir :: FilePath
shakeDir = "._build"

getDate :: UTCTime -> String
getDate = formatTime defaultTimeLocale "%Y - %m%d"


type WithChan r m
    = ( MonadThrow m
      , MonadError App.AppError m
      , MonadReader r m
      , App.Has App.WatchManager r
      , App.Has App.HPhotographers r
      , App.Has App.MPhotographersFile r
      , App.Has App.HTabs r
      , App.Has App.MTabsFile r
      , App.Has App.HShootings r
      , App.Has App.MShootingsFile r
      , App.Has App.HGrades r
      , App.Has App.MGradesFile r
      , App.Has App.HSessions r
      , App.Has App.MSessionsFile r
      , App.Has App.HCameras r
      , App.Has App.MCamerasFile r
      , App.Has App.HDumpDir r
      , App.Has App.HBuild r
      , App.Has App.HConfigDumpDir r
      , App.Has App.MDumpFile r
      , App.Has App.HLocationFile r
      , App.Has App.MLocationFile r
      , App.Has App.HDagsdatoDir r
      , App.Has App.MDagsdatoFile r
      , App.Has App.HDagsdatoBackupDir r
      , App.Has App.MDagsdatoBackupFile r
      , App.Has App.HDoneshootingDir r
      , App.Has App.MDoneshootingFile r
      , App.Has App.MBuildFile r
      , App.Has (App.MStartMap m) r
      , App.Has App.MStopMap r
      , App.Has App.OutChan r
      , App.Has App.InChan r
      , MonadIO m
      , MonadCatch m
      )


getOpts :: forall  r m . WithChan r m => m ShakeOptions
getOpts = return $ shakeOptions { shakeFiles = shakeDir, shakeThreads = 0 }


readDoneshootingDir :: forall  r m . WithChan r m => m [FilePath]
readDoneshootingDir = do
    grades <- App.readGrades
    let grade =
            view (Grade.unGrades . ListZipper.zipperL . Grade.gradeId) grades
    location <- App.readLocation
    let locationName = takeBaseName $ view Location.unLocation location
    cameras <- App.readCameras
    let extension =
            filter ((/=) '.')
                $  Camera.toExtension
                $  cameras
                ^. Camera.unCameras
                .  ListZipper.zipperL
    doneshooting <- App.readDoneshooting
    let doneshootingPath = doneshooting ^. Doneshooting.unDoneshooting
    let path = doneshootingPath </> locationName </> extension </> grade
    files <- liftIO $ tryIOError $ SD.listDirectory path
    return $ either (\_ -> []) (\xs -> xs) files


mkDoneshootingPathJpg
    :: forall  r m . WithChan r m => m (Int -> FilePath -> FilePath)
mkDoneshootingPathJpg = do
    location      <- App.readLocation
    sessions'     <- App.readSessions
    cameras       <- App.readCameras
    photographers <- App.readPhotographers
    doneshooting  <- App.readDoneshooting
    shootings     <- App.readShootings
    grades        <- App.readGrades

    return $ \index file ->
        let locationName = takeBaseName $ view Location.unLocation location
            sessionId =
                    show
                        $  Session.toInteger
                        $  sessions'
                        ^. Session.unSessions
                        .  ListZipper.zipperL

            extension =
                    filter ((/=) '.')
                        $  Camera.toExtension
                        $  cameras
                        ^. Camera.unCameras
                        .  ListZipper.zipperL

            photographerId =
                    photographers
                        ^. Photographer.unPhotographers
                        .  ListZipper.zipperL
                        .  Photographer.tid

            doneshootingPath = doneshooting ^. Doneshooting.unDoneshooting

            session = sessions' ^. Session.unSessions . ListZipper.zipperL
            shootingId = show $ if session == Session.KindergartenGroup
                then 3
                else
                    Shooting.toInteger
                    $  shootings
                    ^. Shooting.unShootings
                    .  ListZipper.zipperL

            grade = view (Grade.unGrades . ListZipper.zipperL) grades
            tid =
                    grade
                        ^. Grade.photographees
                        .  Grade.unPhotographees
                        .  ListZipper.zipperL
                        .  Grade.tid

            fileExtension = toLower <$> (FP.takeExtension file)

            pad x = padLeft '0' 3 (show x)
            no = pad index

            path =
                    doneshootingPath
                        </> locationName
                        </> extension
                        </> "_webshop"
                        </> sessionId
                        ++  "."
                        ++  tid
                        ++  "."
                        ++  shootingId
                        ++  "."
                        ++  photographerId
                        ++  "."
                        ++  no
                        ++  fileExtension
        in  path

mkDoneshootingPath
    :: forall  r m . WithChan r m => m (Int -> FilePath -> FilePath)
mkDoneshootingPath = do
    location      <- App.readLocation
    cameras       <- App.readCameras
    photographers <- App.readPhotographers
    doneshooting  <- App.readDoneshooting
    shootings     <- App.readShootings
    sessions'     <- App.readSessions
    grades        <- App.readGrades

    return $ \index file ->
        let locationName = takeBaseName $ view Location.unLocation location
            sessionId =
                    show
                        $  Session.toInteger
                        $  sessions'
                        ^. Session.unSessions
                        .  ListZipper.zipperL

            extension =
                    filter ((/=) '.')
                        $  Camera.toExtension
                        $  cameras
                        ^. Camera.unCameras
                        .  ListZipper.zipperL

            photographerId =
                    photographers
                        ^. Photographer.unPhotographers
                        .  ListZipper.zipperL
                        .  Photographer.tid

            doneshootingPath = doneshooting ^. Doneshooting.unDoneshooting

            session = sessions' ^. Session.unSessions . ListZipper.zipperL
            shootingId = show $ if session == Session.KindergartenGroup
                then 3
                else
                    Shooting.toInteger
                    $  shootings
                    ^. Shooting.unShootings
                    .  ListZipper.zipperL

            grade   = view (Grade.unGrades . ListZipper.zipperL) grades
            gradeId = view Grade.gradeId grade
            tid =
                    grade
                        ^. Grade.photographees
                        .  Grade.unPhotographees
                        .  ListZipper.zipperL
                        .  Grade.tid

            fileExtension = toLower <$> (FP.takeExtension file)

            pad x = padLeft '0' 3 (show x)
            no = pad index

            path =
                    doneshootingPath
                        </> locationName
                        </> extension
                        </> gradeId
                        </> sessionId
                        ++  "."
                        ++  tid
                        ++  "."
                        ++  shootingId
                        ++  "."
                        ++  photographerId
                        ++  "."
                        ++  no
                        ++  fileExtension
        in  path


mkDagsdatoBackupPath
    :: forall  r m . WithChan r m => m (String -> FilePath -> FilePath)
mkDagsdatoBackupPath = do
    location       <- App.readLocation
    dagsdatoBackup <- App.readDagsdatoBackup
    grades         <- App.readGrades
    return $ \date file ->
        let
            locationName = takeBaseName $ view Location.unLocation location
            grade        = view (Grade.unGrades . ListZipper.zipperL) grades
            gradeId      = view Grade.gradeId grade
            photographee =
                grade
                    ^. Grade.photographees
                    .  Grade.unPhotographees
                    .  ListZipper.zipperL
            tid  = photographee ^. Grade.tid
            name = photographee ^. Grade.name
            dagsdatoBackupPath =
                dagsdatoBackup ^. DagsdatoBackup.unDagsdatoBackup
            fileExtension = toLower <$> (FP.takeExtension file)
            path =
                dagsdatoBackupPath
                    </>  date
                    ++   " - "
                    ++   locationName
                    </>  gradeId
                    </>  (name ++ " - " ++ tid)
                    </>  file
                    -<.> fileExtension
        in
            path

mkDagsdatoPath
    :: forall  r m . WithChan r m => m (String -> FilePath -> FilePath)
mkDagsdatoPath = do
    location <- App.readLocation
    dagsdato <- App.readDagsdato
    grades   <- App.readGrades
    return $ \date file ->
        let
            locationName = takeBaseName $ view Location.unLocation location
            grade        = view (Grade.unGrades . ListZipper.zipperL) grades
            gradeId      = view Grade.gradeId grade
            photographee =
                grade
                    ^. Grade.photographees
                    .  Grade.unPhotographees
                    .  ListZipper.zipperL
            tid           = photographee ^. Grade.tid
            name          = photographee ^. Grade.name
            dagsdatoPath  = dagsdato ^. Dagsdato.unDagsdato
            fileExtension = toLower <$> (FP.takeExtension file)
            path =
                dagsdatoPath
                    </>  date
                    ++   " - "
                    ++   locationName
                    </>  gradeId
                    </>  (name ++ " - " ++ tid)
                    </>  file
                    -<.> fileExtension
        in
            path


padLeft :: Char -> Int -> String -> String
padLeft c n s =
    let len    = length s
        padLen = max 0 (n - len)
        padStr = show $ replicate padLen c
    in  concat [padStr, s]


runBuild :: forall  r m . WithChan r m => m ()
runBuild = do
    time <- liftIO getCurrentTime
    let date = getDate time

    _               <- App.readGrades
    _               <- App.readPhotographers
    _               <- App.readDagsdatoBackup
    _               <- App.readDagsdato
    dumpDir         <- App.readDumpDir
    dump            <- App.readDump
    _               <- App.readDoneshooting


    doneshootingDir <- readDoneshootingDir -- SKAL MODELLERS
    let doneshootingFileCount = length doneshootingDir

    opts                <- getOpts

    doneshootingCrFunc  <- mkDoneshootingPath
    doneshootingJpgFunc <- mkDoneshootingPathJpg
    dagsdatoFunc        <- mkDagsdatoPath
    dagsdatoBackupFunc  <- mkDagsdatoBackupPath
    liftIO $ shake opts $ do
        Indexed.iforM_ (dumpDir ^. DumpDir.unDumpDir) $ \i f -> do

            let crFile            = f ^. DumpDir.cr
            let jpgFile           = f ^. DumpDir.jpg

            let index'            = doneshootingFileCount + i + 1

            let doneshootingCr    = doneshootingCrFunc index' crFile
            let doneshootingJpg   = doneshootingJpgFunc index' jpgFile

            let dagsdatoCr        = dagsdatoFunc date crFile
            let dagsdatoJpg       = dagsdatoFunc date jpgFile

            let dagsdatoBackupCr  = dagsdatoBackupFunc date crFile
            let dagsdatoBackupJpg = dagsdatoBackupFunc date jpgFile

            want
                [ doneshootingCr
                , doneshootingJpg
                , dagsdatoCr
                , dagsdatoJpg
                , dagsdatoBackupCr
                , dagsdatoBackupJpg
                ]

            doneshootingCr %> copyFile' crFile

            doneshootingJpg %> copyFile' jpgFile

            dagsdatoCr %> copyFile' crFile

            dagsdatoJpg %> copyFile' jpgFile

            dagsdatoBackupCr %> copyFile' crFile

            dagsdatoBackupJpg %> copyFile' jpgFile

            action $ removeFilesAfter
                (dump ^. Dump.unDump)
                [ "//*.CR3"
                , "//*.JPG"
                , "//*.cr3"
                , "//*.jpg"
                , "//*.CR2"
                , "//*.cr2"
                ]
