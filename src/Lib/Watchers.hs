module Lib.Watchers
    ( photographersFile
    )
where

import           Lib.App                        ( AppEnv
                                                , Env(..)
                                                , StopMap
                                                , grab
                                                , obtain
                                                , Has(..)
                                                , MStartMap(..)
                                                , MStopMap(..)
                                                , OutChan(..)
                                                , InChan(..)
                                                , WatchManager(..)
                                                , MPhotographersFile
                                                , unMPhotographersFile
                                                , unMStopMap
                                                , unHPhotographers
                                                , unMStartMap
                                                , unOutChan
                                                , unInChan
                                                )
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan
import qualified System.FSNotify               as FS
import qualified Lib.Message                   as Message
import qualified System.FilePath               as FP


type WithEnv r m = (MonadReader r m, Has MPhotographersFile r, Has WatchManager r, Has (MStartMap m) r, Has MStopMap r, Has OutChan r, Has InChan r, MonadIO m)


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
