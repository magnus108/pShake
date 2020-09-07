module Lib.Server
    ( setup
    )
where

import Control.Monad.Catch (MonadThrow)
import qualified Data.HashMap.Strict           as HashMap
import qualified Lib.Message                   as Message



import qualified Lib.Watchers                  as Watchers
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import           Lib.App                        ( AppEnv
                                                , runAppAsIO
                                                , runApp
                                                , Env(..)
                                                , StopMap
                                                , grab
                                                , obtain
                                                , Has(..)
                                                , HPhotographers(..)
                                                , MPhotographersFile(..)
                                                , MStartMap(..)
                                                , WatchManager(..)
                                                , MStopMap(..)
                                                , OutChan(..)
                                                , InChan(..)
                                                , unMPhotographersFile
                                                , unMStopMap
                                                , unHPhotographers
                                                , unMStartMap
                                                , unOutChan
                                                , unInChan
                                                )
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import qualified Lib.Model.Photographer        as Photographer


setup :: AppEnv -> Window -> UI ()
setup env win = do
    _       <- return win # set title "FF"
    content <- UI.p # set text "bob"
    void $ UI.getBody win # set children [content]

    _ <- liftIO $ runApp env setupStartMap
    _ <- liftIO $ runApp env startStartMap

    messageReceiver <- liftIO $ forkIO $ runApp env (receiveMessages win)

    UI.on UI.disconnect win $ const $ liftIO $ do
        --HashMap ti list and kill all
        killThread messageReceiver
        return ()


type WithChan r m = (MonadThrow m, MonadReader r m, Has WatchManager r, Has HPhotographers r, Has MPhotographersFile r, Has (MStartMap m) r, Has MStopMap r, Has OutChan r, Has InChan r, MonadIO m)


setupStartMap :: forall r m. WithChan r m => m ()
setupStartMap = do
    mStartMap <- unMStartMap <$> grab @(MStartMap m)
    startMap <- takeMVar mStartMap
    let key         = "photographers"
    let value       = Watchers.photographersFile
    let newStartMap = HashMap.insert key value startMap
    putMVar mStartMap newStartMap


startStartMap :: forall r m. WithChan r m => m ()
startStartMap = do
    inChan <- unInChan <$> grab @InChan
    liftIO $ Chan.writeChan inChan Message.StartPhotograpers


receiveMessages :: forall r m. WithChan r m => Window -> m ()
receiveMessages window = do
    outChan <- grab @OutChan
    messages <- liftIO $ Chan.getChanContents (unOutChan outChan)
    hPhotographers <- unHPhotographers <$> grab @HPhotographers
    forM_ messages $ \x -> do
        traceShowM x
        case x of
            Message.StopPhotographers -> do
                return ()
            Message.StartPhotograpers -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap <- unMStopMap <$> grab @MStopMap
                stopMap <- takeMVar mStopMap
                startMap <- takeMVar mStartMap
                let key   = "photographers"
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadPhotographers -> do
                mPhotographersFile <- unMPhotographersFile <$> grab @MPhotographersFile
                photographersFile <- takeMVar mPhotographersFile
                photographers <- Photographer.getPhotographers photographersFile
                _             <- putMVar mPhotographersFile photographersFile
                _ <- liftIO $ hPhotographers photographers
                _ <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()
