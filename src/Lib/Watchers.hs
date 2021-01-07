module Lib.Watchers
    ( photographersFile
    , tabsFile
    , shootingsFile
    , dumpFile
    , dagsdatoFile
    , camerasFile
    )
where

import           Lib.App                        ( grab
                                                , Has(..)
                                                , InChan
                                                , OutChan
                                                , WatchManager
                                                , MPhotographersFile
                                                , MTabsFile

                                                , MShootingsFile
                                                , MCamerasFile
                                                , MDumpFile
                                                , MDagsdatoFile

                                                , MStopMap
                                                , MStartMap
                                                , unMPhotographersFile
                                                , unMTabsFile

                                                , unMShootingsFile
                                                , unMCamerasFile
                                                , unMDumpFile
                                                , unMDagsdatoFile

                                                , unInChan
                                                , unWatchManager
                                                )
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan
import qualified System.FSNotify               as FS
import qualified Lib.Message                   as Message
import qualified System.FilePath               as FP


type WithEnv r m = (MonadReader r m
  , Has MPhotographersFile r

  , Has MTabsFile r
  , Has MShootingsFile r
  , Has MCamerasFile r

  , Has MDumpFile r

  , Has MDagsdatoFile r

  , Has WatchManager r, Has (MStartMap m) r, Has MStopMap r, Has OutChan r, Has InChan r, MonadIO m)


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
