module Lib.Server
    ( runServer
    )
where

import qualified Data.HashMap.Strict           as HashMap
import qualified Lib.Message                   as Message
import qualified Lib.Watchers                  as Watchers
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import           Lib.App                        ( Env(..) )
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import qualified Lib.Model.Photographer        as Photographer


runServer :: MonadIO m => Env IO Message.Message -> m ()
runServer env@Env {..} = liftIO $ do
    startGUI defaultConfig { jsWindowReloadOnDisconnect = False
                           , jsPort                     = Just port
                           , jsStatic                   = Just static
                           , jsCustomHTML               = Just index
                           }
        $ setup env


setup :: Env IO Message.Message -> Window -> UI ()
setup env@Env {..} win = do
    _       <- return win # set title "FF"
    content <- UI.p # set text "bob"
    void $ UI.getBody win # set children [content]

    startMap <- takeMVar mStartMap
    let key         = "photographers"
    let value       = Watchers.photographersFile
    let newStartMap = HashMap.insert key value startMap
    _               <- putMVar mStartMap newStartMap

    messageReceiver <- liftIO $ forkIO $ receiveMessages env win

    --testing
    liftIO $ Chan.writeChan inChan Message.StartPhotograpers

    UI.on UI.disconnect win $ const $ liftIO $ do
        killThread messageReceiver


receiveMessages :: Env IO Message.Message -> Window -> IO ()
receiveMessages env@Env {..} window = do
    messages <- Chan.getChanContents outChan
    forM_ messages $ \x -> do
        case x of
            Message.StopPhotographers -> do
                return ()
            Message.StartPhotograpers -> do
                startMap <- takeMVar mStartMap
                stopMap  <- takeMVar mStopMap
                let key   = "photographers"
                let watch = startMap HashMap.! key
                stop <- watch env
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadPhotographers -> do
                photographersFile <- takeMVar mPhotographersFile
                photographers <- Photographer.getPhotographers photographersFile
                _ <- putMVar mPhotographersFile photographersFile
                hPhotographees photographers

                runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed

                print photographers
                return ()
