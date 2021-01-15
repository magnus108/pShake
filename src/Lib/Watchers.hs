module Lib.Watchers
    ( photographersFile
    , tabsFile
    , shootingsFile
    , sessionsFile
    , dumpFile
    , dumpDir
    , gradesFile
    , dagsdatoFile
    , dagsdatoBackupFile
    , doneshootingFile
    , camerasFile
    , locationFile
    )
where

import           Lib.App                        ( grab
                                                , Has(..)
                                                , InChan
                                                , OutChan
                                                , WatchManager
                                                , MPhotographersFile
                                                , MTabsFile

                                                , MSessionsFile
                                                , MShootingsFile
                                                , MCamerasFile
                                                , MDumpFile
                                                , MGradesFile
                                                , MDagsdatoFile
                                                , MDagsdatoBackupFile
                                                , MDoneshootingFile
                                                , MLocationFile

                                                , MStopMap
                                                , MStartMap
                                                , unMPhotographersFile
                                                , unMTabsFile

                                                , unMShootingsFile
                                                , unMSessionsFile
                                                , unMCamerasFile
                                                , unMGradesFile
                                                , unMDumpFile
                                                , unMDagsdatoFile
                                                , unMDagsdatoBackupFile
                                                , unMDoneshootingFile
                                                , unMLocationFile

                                                , unInChan
                                                , unWatchManager
                                                )
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan
import qualified System.FSNotify               as FS
import qualified Lib.Message                   as Message
import qualified System.FilePath               as FP


import qualified Lib.Model.Dump                as Dump

import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                )

type WithEnv r m = (MonadReader r m
  , Has MPhotographersFile r
  , Has MGradesFile r

  , Has MTabsFile r
  , Has MShootingsFile r
  , Has MSessionsFile r
  , Has MCamerasFile r

  , Has MDumpFile r

  , Has MDagsdatoFile r
  , Has MDagsdatoBackupFile r
  , Has MDoneshootingFile r
  , Has MLocationFile r

  , Has WatchManager r, Has (MStartMap m) r, Has MStopMap r, Has OutChan r, Has InChan r, MonadIO m,
    MonadCatch m, MonadThrow m)



photographersFile :: WithEnv r m => m FS.StopListening
photographersFile = do
    unMPhotographersFile <- unMPhotographersFile <$> grab @MPhotographersFile
    file <- liftIO $ readMVar unMPhotographersFile
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadPhotographers
            return ()
        )
    return stop

tabsFile :: WithEnv r m => m FS.StopListening
tabsFile = do
    unMTabsFile <- unMTabsFile <$> grab @MTabsFile
    file <- liftIO $ readMVar unMTabsFile
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadTabs
            return ()
        )
    return stop


camerasFile :: WithEnv r m => m FS.StopListening
camerasFile = do
    unMCamerasFile <- unMCamerasFile <$> grab @MCamerasFile
    file <- liftIO $ readMVar unMCamerasFile
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadCameras
            return ()
        )
    return stop


shootingsFile :: WithEnv r m => m FS.StopListening
shootingsFile = do
    unMShootingsFile <- unMShootingsFile <$> grab @MShootingsFile
    file <- liftIO $ readMVar unMShootingsFile
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadShootings
            return ()
        )
    return stop


sessionsFile :: WithEnv r m => m FS.StopListening
sessionsFile = do
    unMSessionsFile <- unMSessionsFile <$> grab @MSessionsFile
    file <- liftIO $ readMVar unMSessionsFile
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadSessions
            return ()
        )
    return stop


dumpFile :: WithEnv r m => m FS.StopListening
dumpFile = do
    unMDumpFile <- unMDumpFile <$> grab @MDumpFile
    file <- liftIO $ readMVar unMDumpFile
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadDump
            Chan.writeChan inChan Message.StopDumpDir
            Chan.writeChan inChan Message.StartDumpDir
            Chan.writeChan inChan Message.ReadDumpDir
            return ()
        )
    return stop


dumpDir :: WithEnv r m => m FS.StopListening
dumpDir = do
    unMDumpFile <- unMDumpFile <$> grab @MDumpFile
    file <- liftIO $ readMVar unMDumpFile
    dump <- Dump.getDump file
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (Dump.unDump dump )
        (const True)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadDumpDir
            return ()
        )
    return stop

dagsdatoFile :: WithEnv r m => m FS.StopListening
dagsdatoFile = do
    unMDagsdatoFile <- unMDagsdatoFile <$> grab @MDagsdatoFile
    file <- liftIO $ readMVar unMDagsdatoFile
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadDagsdato
            return ()
        )
    return stop

dagsdatoBackupFile :: WithEnv r m => m FS.StopListening
dagsdatoBackupFile = do
    unMDagsdatoBackupFile <- unMDagsdatoBackupFile <$> grab @MDagsdatoBackupFile
    file <- liftIO $ readMVar unMDagsdatoBackupFile
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadDagsdatoBackup
            return ()
        )
    return stop


doneshootingFile :: WithEnv r m => m FS.StopListening
doneshootingFile = do
    unMDoneshootingFile <- unMDoneshootingFile <$> grab @MDoneshootingFile
    file <- liftIO $ readMVar unMDoneshootingFile
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadDoneshooting
            return ()
        )
    return stop


locationFile :: WithEnv r m => m FS.StopListening
locationFile = do
    unMLocationFile <- unMLocationFile <$> grab @MLocationFile
    file <- liftIO $ readMVar unMLocationFile
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadLocation
            return ()
        )
    return stop

    
gradesFile :: WithEnv r m => m FS.StopListening
gradesFile = do
    unMGradesFile <- unMGradesFile <$> grab @MGradesFile
    file <- liftIO $ readMVar unMGradesFile
    watchManager <- unWatchManager <$> grab @WatchManager
    inChan <- unInChan <$> grab @InChan
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan inChan Message.ReadGrades
            return ()
        )
    return stop
