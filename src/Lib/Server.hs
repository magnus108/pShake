module Lib.Server
    ( setup
    )
where

import qualified Data.HashMap.Strict           as HashMap
import qualified Lib.Message                   as Message
import qualified Lib.Watchers                  as Watchers
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import           Lib.App                        ( AppEnv
                                                , Env(..)
                                                , grab
                                                , obtain
                                                , MStartMap(..)
                                                , OutChan(..)
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

    {-
    mStartMap <- grab @MStartMap
    startMap <- takeMVar $ unMStartMap mStartMap
    let key         = "photographers"
    let value       = Watchers.photographersFile
    let newStartMap = HashMap.insert key value startMap
    _               <- putMVar mStartMap newStartMap
    -}

    return ()
    {-


    messageReceiver <- liftIO $ forkIO $ receiveMessages env win

    --testing
    liftIO $ Chan.writeChan (unInChan inChan) Message.StartPhotograpers

    UI.on UI.disconnect win $ const $ liftIO $ do
        --HashMap ti list and kill all
        killThread messageReceiver


-}
receiveMessages :: AppEnv -> Window -> IO ()
receiveMessages env window = do
    outChan <- grab @OutChan
--    messages <- Chan.getChanContents (unOutChan outChan)
    return ()
        {-
    forM_ messages $ \x -> do
        case x of
            Message.StopPhotographers -> do
                return ()
            Message.StartPhotograpers -> do
                startMap <- takeMVar $ unMStartMap mStartMap
                stopMap  <- takeMVar $ unMStopMap mStopMap
                let key   = "photographers"
                let watch = startMap HashMap.! key
                stop <- watch env
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar (unMStartMap mStartMap) startMap
                _ <- putMVar (unMStopMap mStopMap) newStopMap
                return ()

            Message.ReadPhotographers -> do
                photographersFile <- takeMVar
                    $ unMPhotographersFile mPhotographersFile
                photographers <- Photographer.getPhotographers photographersFile
                _             <- putMVar (unMPhotographersFile mPhotographersFile) photographersFile
                _ <- (unHPhotographers hPhotographers) photographers

                runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed

                print photographers
                return ()
                -}
