module Lib.Watchers
    ( photographersFile
    )
where

import           Lib.App                        ( Env(..)
                                                , unMPhotographersFile
                                                , unInChan
                                                )
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan
import qualified System.FSNotify               as FS
import qualified Lib.Message                   as Message
import qualified System.FilePath               as FP


photographersFile :: Env m -> IO FS.StopListening
photographersFile Env {..} = do
    file <- readMVar $ unMPhotographersFile mPhotographersFile
    stop <- liftIO $ FS.watchDir
        watchManager
        (FP.dropFileName file)
        (\e -> FP.takeFileName (FS.eventPath e) == file)
        (\e -> void $ do
            print e
            Chan.writeChan (unInChan inChan) Message.ReadPhotographers
        )
    return stop
