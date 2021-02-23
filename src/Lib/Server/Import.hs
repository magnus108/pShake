module Lib.Server.Import
    ( runImport
    )
where


import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString               as B


import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

import           Lib.App                        ( MPhotographersFile
                                                , InChan
                                                , Has
                                                , OutChan
                                                , MStartMap
                                                , MStopMap
                                                , MBuildFile
                                                , MDoneshootingFile
                                                , MDagsdatoFile
                                                , MShootingsFile
                                                , HDoneshootingDir
                                                , MDagsdatoBackupFile
                                                , HPhotographers
                                                , HDagsdatoBackupDir
                                                , HDagsdatoDir
                                                , MLocationFile
                                                , HLocationFile
                                                , MDumpFile
                                                , HConfigDumpDir
                                                , HBuild
                                                , HDumpDir
                                                , HCameras
                                                , MCamerasFile
                                                , MSessionsFile
                                                , HSessions
                                                , HGrades
                                                , MGradesFile
                                                , HTabs
                                                , HShootings
                                                , MTabsFile
                                                , WatchManager
                                                , AppError
                                                , unMPhotographersFile
                                                , grab
                                                )
import qualified Codec.Archive.Zip             as Zip


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


runImport :: forall  r m . WithChan r m => String -> m ()
runImport file = do
    bs <- liftIO $ B.readFile file
    let archive = Zip.toArchive (BL.fromStrict bs)
    mPhotographersFile <- unMPhotographersFile <$> grab @MPhotographersFile
    photographersFile  <- readMVar mPhotographersFile
    let maybePhotographers = Zip.findEntryByPath photographersFile archive
    let maybeBS            = Zip.fromEntry <$> maybePhotographers
    liftIO $ forM_ maybeBS $ \bs' ->
        B.writeFile photographersFile (BL.toStrict bs')
