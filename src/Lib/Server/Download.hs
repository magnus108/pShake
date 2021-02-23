module Lib.Server.Download
    ( runDownload
    )
where


import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString               as B



import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )


import qualified Lib.App                       as App

import qualified Codec.Archive.Zip             as Zip


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

runDownload :: forall  r m . WithChan r m => String -> m ()
runDownload file = do
    let empty' = Zip.emptyArchive
    mPhotographersFile <- App.unMPhotographersFile <$> App.grab @App.MPhotographersFile
    photographersFile <- readMVar mPhotographersFile
    archive1 <- liftIO $ Zip.addFilesToArchive [] empty' [photographersFile]
    liftIO $ B.writeFile file (BL.toStrict (Zip.fromArchive archive1))
