{-# LANGUAGE RecursiveDo #-}
module Lib.Server
    ( setup
    )
where

import           Control.Monad.Catch            ( MonadThrow )
import qualified Data.HashMap.Strict           as HashMap
import qualified Lib.Message                   as Message



import qualified Lib.Watchers                  as Watchers
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import           Lib.App                        ( runApp
                                                , AppEnv
                                                , grab
                                                , Env(..)
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
import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Model                     as Model


data PhotographerEntry = PhotographerEntry
    { _elementTE :: !Element
    , _photographersTE    :: !(Tidings (Model.Data String Photographer.Photographers))
    }

instance Widget PhotographerEntry where
    getElement = _elementTE

entry
    :: Behavior (Model.Data String Photographer.Photographers)
    -> UI PhotographerEntry
entry bValue = do
    content <- UI.div

--   bEditing <- stepper False $ and <$>
--        unions [True <$ UI.focus input, False <$ UI.blur input]

    window  <- askWindow
    liftIOLater $ Reactive.onChange bValue $ \s -> runUI window $ do
        case s of
            x ->
                --editing <- liftIO $ currentValue bEditing
                --when (not editing) $ void $ element input # set value s
                void $ element content # set text "bobo"

    let _elementTE       = content
        _photographersTE = tidings bValue $ UI.never
    return PhotographerEntry { .. }



setup :: AppEnv -> Window -> UI ()
setup env@Env {..} win = mdo
    _       <- return win # set title "FF"
    content <- UI.p # set text "bob"
    elem <- entry bPhotographers

    void $ UI.getBody win #+ [element elem]

    _               <- liftIO $ runApp env setupStartMap
    _               <- liftIO $ runApp env startStartMap

    messageReceiver <- liftIO $ forkIO $ runApp env (receiveMessages win)


    UI.on UI.disconnect win $ const $ liftIO $ do
        --HashMap ti list and kill all
        killThread messageReceiver
        return ()


type WithChan r m
    = ( MonadThrow m
      , MonadReader r m
      , Has WatchManager r
      , Has HPhotographers r
      , Has MPhotographersFile r
      , Has (MStartMap m) r
      , Has MStopMap r
      , Has OutChan r
      , Has InChan r
      , MonadIO m
      )


setupStartMap :: forall  r m . WithChan r m => m ()
setupStartMap = do
    mStartMap <- unMStartMap <$> grab @(MStartMap m)
    startMap  <- takeMVar mStartMap
    let key         = "photographers"
    let watcher     = Watchers.photographersFile
    let newStartMap = HashMap.insert key watcher startMap
    putMVar mStartMap newStartMap


startStartMap :: forall  r m . WithChan r m => m ()
startStartMap = do
    inChan <- unInChan <$> grab @InChan
    liftIO $ Chan.writeChan inChan Message.StartPhotograpers


receiveMessages :: forall  r m . WithChan r m => Window -> m ()
receiveMessages window = do
    outChan        <- grab @OutChan
    messages       <- liftIO $ Chan.getChanContents (unOutChan outChan)
    hPhotographers <- unHPhotographers <$> grab @HPhotographers
    forM_ messages $ \x -> do
    ---    traceShowM x
        case x of
            Message.StopPhotographers -> do
                return ()
            Message.StartPhotograpers -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key   = "photographers"
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadPhotographers -> do
                mPhotographersFile <-
                    unMPhotographersFile <$> grab @MPhotographersFile
                photographersFile <- takeMVar mPhotographersFile
                photographers <- Photographer.getPhotographers photographersFile
                _ <- putMVar mPhotographersFile photographersFile
                _ <- liftIO $ hPhotographers $ Model.Data photographers
                _ <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()
