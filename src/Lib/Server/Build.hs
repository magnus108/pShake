module Lib.Server.Build
    ( runBuild
    ) where

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

        {-

import qualified System.Directory as SD
import System.Directory
import qualified Lib.App as App

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Chan.Strict as Chan

import Data.Char

import Control.Exception

import Utils.Mealy

import System.Time.Extra


import qualified Data.List.Index
import Data.Strings

import Utils.Comonad

import qualified Lib.Main as Main
import qualified Control.Lens as Lens

import qualified Lib.Build as Build
import qualified Lib.Shooting as Shooting
import qualified Lib.Photographer as Photographer
import qualified Lib.Photographee as Photographee
import qualified Lib.Session as Session
import qualified Lib.Location as Location
import qualified Lib.Grade as Grade
import qualified Lib.Dump as Dump
import qualified Lib.Doneshooting as Doneshooting
import qualified Lib.Dagsdato as Dagsdato
import qualified Lib.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Camera as Camera

import Development.Shake
import Development.Shake.FilePath 
import qualified Development.Shake.FilePath as FP

import Data.Time.Format
import Data.Time.Clock










shakeDir :: FilePath
shakeDir = "._build"

getDate :: UTCTime -> String
getDate = formatTime defaultTimeLocale "%Y - %m%d"


message :: Mealy (Double, Progress) (Double, Progress) -> Mealy (Double, Progress) (Double, Int)
message input = liftA2 (,) done todo
    where
        progress = snd <$> input
        _ = fst <$> input
        done = timeBuilt <$> progress
        todo = countBuilt <$> progress



myProgressProgram :: Int -> Chan.Chan App.Action -> Photographee.Photographee -> IO Progress -> IO ()
myProgressProgram sample c photographee progress = do
    time <- offsetTime
    catchJust (\x -> if x == ThreadKilled then Just () else Nothing)
        (loop time $ message echoMealy)
        (const $ do _ <- time
                    p <- progress
                    let todo = countBuilt p
                    Chan.writeChan c (App.BuilderMessage (Build.DoneBuild photographee (show (div todo 8))))
        )
    where
        loop :: IO Double -> Mealy (Double, Progress) (Double, Int) -> IO ()
        loop time mealy = do
            threadDelay sample
            t <- time
            p <- progress
            ((_,todo), mealyy) <- pure $ runMealy mealy (t, p)
            let f = isFailure p
            case f of
                Nothing -> do
                    Chan.writeChan c (App.BuilderMessage (Build.Building photographee (show (div todo 8))))
                Just _ ->
                    Chan.writeChan c (App.BuilderMessage (Build.NoBuild))
            loop time mealyy


opts :: Chan.Chan App.Action -> Photographee.Photographee -> ShakeOptions
opts  c photographee = shakeOptions
                    { shakeFiles = shakeDir
                    , shakeProgress = progress -- should change
                    , shakeThreads = 0
                    }
    where
        progress p = do
            myProgressProgram 1000000 c photographee p

gradePath :: Main.Item -> FilePath
gradePath item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> grade
        where
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            session = Lens.view Main.session item
            sessionId = show $ Session.toInteger session

            camera = Lens.view Main.camera item
            extension = snd $ Camera.toExtension camera
            photographer = Lens.view Main.photographer item
            photographerId = Lens.view Photographer.tid photographer
            doneshooting = Lens.view Main.doneshooting item
            shooting = Lens.view Main.shooting item
            shootingId = if session == Session.KindergartenGroup then "3" else show $ Shooting.toInteger shooting
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee


mkDoneshootingPath :: Int -> FilePath -> Main.Item -> FilePath
mkDoneshootingPath index' file item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> grade </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ (toLower <$> (FP.takeExtension file))
        where
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            session = Lens.view Main.session item
            sessionId = show $ Session.toInteger session

            camera = Lens.view Main.camera item
            extension = snd $ Camera.toExtension camera
            photographer = Lens.view Main.photographer item
            photographerId = Lens.view Photographer.tid photographer
            doneshooting = Lens.view Main.doneshooting item
            shooting = Lens.view Main.shooting item
            shootingId = if session == Session.KindergartenGroup then "3" else show $ Shooting.toInteger shooting
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee
            pad x = Strings.strPadLeft '0' 3 (show x)
            no = pad index


mkDoneshootingPathJpg :: Int -> FilePath -> Main.Item -> FilePath
mkDoneshootingPathJpg index' file item =
    Doneshooting.unDoneshooting doneshooting </> location </> extension </> "_webshop" </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ photographerId ++ "." ++ no ++ (toLower <$> (FP.takeExtension file))
        where
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            session = Lens.view Main.session item
            sessionId = show $ Session.toInteger session

            camera = Lens.view Main.camera item
            extension = snd $ Camera.toExtension camera
            photographer = Lens.view Main.photographer item
            photographerId = Lens.view Photographer.tid photographer
            doneshooting = Lens.view Main.doneshooting item
            shooting = Lens.view Main.shooting item
            shootingId = if session == Session.KindergartenGroup then "3" else show $ Shooting.toInteger shooting
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee
            pad x = strPadLeft '0' 3 (show x)
            no = pad index'


mkDagsdatoPath :: FilePath -> String -> Main.Item -> FilePath
mkDagsdatoPath file date item = dagsdato </> date ++ " - " ++ location </> grade </> (name ++ " - " ++ tea) </> file -<.> (toLower <$> (FP.takeExtension file))
        where
            dagsdato = Dagsdato.unDagsdato $ Lens.view Main.dagsdato item
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            tea = Photographee.toTea' photographee
            name = Photographee.toName' photographee


mkDagsdatoBackupPath :: FilePath -> String -> Main.Item -> FilePath
mkDagsdatoBackupPath file date item = dagsdatoBackup </> date ++ " - " ++ location </> grade </> (name ++ " - " ++ tea) </> file -<.> (toLower <$> (FP.takeExtension file))
        where
            dagsdatoBackup = DagsdatoBackup.unDagsdatoBackup $ Lens.view Main.dagsdatoBackup item
            location = takeBaseName $ Location.unLocationFile $ Lens.view Main.location item
            grade = Grade.showGrade (Lens.view Main.grades item)
            photographees = Lens.view Main.photographees item
            photographee = extract (Photographee.unPhotographees photographees)
            name   = Photographee.toName' photographee
            tea   = Photographee.toTea' photographee


entry :: Chan.Chan App.Action -> MVar FilePath -> MVar FilePath -> Main.Item -> IO ()
entry messages mBuildFile mDumpFile item = do
    time <- getCurrentTime
    let date = getDate time

    let photographees = Lens.view Main.photographees item
    let photographee = extract (Photographee.unPhotographees photographees)

    shaken <- try $ myShake (opts messages photographee) date item :: IO (Either SomeException ())
    case shaken of
        Left e ->  do
            case show e of
                "user error (missingjpg)" ->  do
                    Chan.writeChan messages (App.BuilderMessage (Build.NoJpgBuild))
                _ ->
                    Chan.writeChan messages (App.BuilderMessage (Build.NoBuild))
        Right _ -> do
            Chan.writeChan messages (App.BuilderMessage (Build.DoneBuild photographee ("")))
            --HACK
            let dump = Lens.view Main.dump item
            Chan.writeChan messages (App.WriteDump dump)



myShake :: ShakeOptions -> String -> Main.Item -> IO ()
myShake opts' time item = do
    let dump = Lens.view Main.dump item
    let root = Dump.unDump dump
    let dumpDir = Lens.view Main.dumpDir item
    let sortDir = sort (Dump.unDumpDir dumpDir)
    let pathCR = gradePath item
    files <- try $ listDirectory pathCR :: IO (Either SomeException [FilePath])
    --traceShowM files
    let count = case files of
            Left e -> []
            Right filess -> filess

    let iindex = length count

    tmp <- case sortDir  of
            [] -> error "empty"
            xss ->  Data.List.Index.imapM (\index' cr -> do 

                let index'' = iindex + index' + 1
                let jpg = cr -<.> "jpg"
                eh <- SD.doesFileExist (root </> jpg)
                case eh of
                    True -> do
                        let doneshootingCr = mkDoneshootingPath index'' cr item
                        let doneshootingJpg = mkDoneshootingPathJpg index'' jpg item

                        let dagsdatoCr = mkDagsdatoPath cr time item
                        let dagsdatoJpg = mkDagsdatoPath jpg time item

                        let dagsdatoBackupCr = mkDagsdatoBackupPath cr time item
                        let dagsdatoBackupJpg = mkDagsdatoBackupPath jpg time item
                        return ((cr, (doneshootingCr,dagsdatoCr, dagsdatoBackupCr)), (jpg, (doneshootingJpg, dagsdatoJpg, dagsdatoBackupJpg)))
                    False -> fail "missingjpg"
                ) xss

    case tmp of
        [] -> error "empty"
        xs -> do
            shake opts' $ do
                forM_ xs $ \ ((cr, (doneshootingCr,dagsdatoCr, dagsdatoBackupCr)),(jpg, (doneshootingJpg, dagsdatoJpg, dagsdatoBackupJpg))) -> do

                    want [doneshootingCr, doneshootingJpg, dagsdatoCr, dagsdatoJpg , dagsdatoBackupCr, dagsdatoBackupJpg]

                    doneshootingCr %> copyFile' (root </> cr)

                    doneshootingJpg %> copyFile' (root </> jpg)

                    dagsdatoCr %> copyFile' (root </> cr)

                    dagsdatoJpg %> copyFile' (root </> jpg)

                    dagsdatoBackupCr %> copyFile' (root </> cr)

                    dagsdatoBackupJpg %> copyFile' (root </> jpg)

                    action $ removeFilesAfter root ["//*.CR3", "//*.JPG", "//*.cr3", "//*.jpg","//*.CR2","//*.cr2"]
                    -}


-- runBuild :: Chan.Chan App.Action -> MVar FilePath -> MVar FilePath -> Main.Item -> IO ()


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
    let extension= Camera.toExtension $ cameras ^. Camera.unCameras . ListZipper.zipperL
    photographers <- readPhotographers
    let photographerId = photographers ^. Photographer.unPhotographers . ListZipper.zipperL . Photographer.tid
    shootings <- readShootings
    sessions <- readSessions
    let session = sessions ^. Session.unSessions . ListZipper.zipperL
    let shootingId = show $ if session == Session.KindergartenGroup then 3 else Shooting.toInteger $ shootings ^. Shooting.unShootings . ListZipper.zipperL
    doneshooting <- readDoneshooting
    let doneshootingPath = doneshooting ^. Doneshooting.unDoneshooting
    let path = doneshootingPath </> locationName </> extension </> grade
    files <- liftIO $ SD.listDirectory path
    return files


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

            extension = Camera.toExtension $ cameras ^. Camera.unCameras . ListZipper.zipperL

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

            extension = Camera.toExtension $ cameras ^. Camera.unCameras . ListZipper.zipperL

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
    doneshooting <- readDoneshooting


    doneshootingDir <- readDoneshootingDir -- SKAL MODELLERS
    let doneshootingFileCount = length doneshootingDir

    opts <- getOpts

    doneshootingCrFunc <- mkDoneshootingPath
    doneshootingJpgFunc <- mkDoneshootingPathJpg
    dagsdatoFunc <- mkDagsdatoPath
    dagsdatoBackupFunc <- mkDagsdatoBackupPath
    liftIO $ shake opts $ do
        Indexed.iforM_ (dumpDir ^. DumpDir.unDumpDir) $ \ i f -> do -- pair 

                let crFile = f ^. DumpDir.cr
                let jpgFile = f ^. DumpDir.jpg

                let index' = doneshootingFileCount + i + 1

                let doneshootingCr = doneshootingCrFunc index' crFile
                let doneshootingJpg = doneshootingJpgFunc index' jpgFile

                let dagsdatoCr = dagsdatoFunc date crFile
                let dagsdatoJpg = dagsdatoFunc date jpgFile

                let dagsdatoBackupCr = dagsdatoBackupFunc date crFile
                let dagsdatoBackupJpg = dagsdatoBackupFunc date jpgFile

                want []
            {-
            ((cr, (doneshootingCr,dagsdatoCr, dagsdatoBackupCr)),(jpg, (doneshootingJpg, dagsdatoJpg, dagsdatoBackupJpg))) -> do

            want [doneshootingCr, doneshootingJpg, dagsdatoCr, dagsdatoJpg , dagsdatoBackupCr, dagsdatoBackupJpg]

            doneshootingCr %> copyFile' (root </> cr)

            doneshootingJpg %> copyFile' (root </> jpg)

            dagsdatoCr %> copyFile' (root </> cr)

            dagsdatoJpg %> copyFile' (root </> jpg)

            dagsdatoBackupCr %> copyFile' (root </> cr)

            dagsdatoBackupJpg %> copyFile' (root </> jpg)

            action $ removeFilesAfter root ["//*.CR3", "//*.JPG", "//*.cr3", "//*.jpg","//*.CR2","//*.cr2"]
            -}
