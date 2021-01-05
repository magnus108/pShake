{-# LANGUAGE RecursiveDo #-}
module Lib.Server
    ( setup
    )
where

import           System.IO.Error                ( IOError
                                                , isUserError
                                                )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , catch
                                                , catchIOError
                                                , catchAll
                                                , try
                                                )
import qualified Control.Monad.Except          as E
                                                ( catchError
                                                , throwError
                                                )
import qualified Data.HashMap.Strict           as HashMap
import qualified Lib.Message                   as Message
import           Utils.Comonad
import qualified Utils.ListZipper              as ListZipper
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                )
import qualified Control.Lens                  as Lens


import qualified Lib.Watchers                  as Watchers
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import           Lib.App                        ( runApp
                                                , AppEnv
                                                , grab
                                                , AppException(..)
                                                , AppError(..)
                                                , WithError
                                                , IError(..)
                                                , Env(..)
                                                , Has(..)
                                                , HPhotographers(..)
                                                , MPhotographersFile(..)
                                                , HTabs(..)
                                                , MTabsFile(..)
                                                , MStartMap(..)
                                                , WatchManager(..)
                                                , MStopMap(..)
                                                , OutChan(..)
                                                , InChan(..)
                                                , unMPhotographersFile
                                                , unHPhotographers
                                                , unMTabsFile
                                                , unHTabs
                                                , unMStopMap
                                                , unMStartMap
                                                , unOutChan
                                                , unInChan
                                                )
import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Control.Concurrent.Chan.Unagi.Bounded
                                               as Chan

import qualified Lib.Model.Tab                 as Tab
import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Model.Data                as Data


data PhotographerEntry = PhotographerEntry
    { _elementTE :: !Element
    , _photographersTE    :: !(Event Photographer.Photographers)
    }

instance Widget PhotographerEntry where
    getElement = _elementTE

entry
    :: Behavior (Data.Data String Photographer.Photographers)
    -> UI PhotographerEntry
entry bValue = do
    content  <- UI.div
    input    <- UI.input
    bEditing <- stepper False $ and <$> unions
        [True <$ UI.focus input, False <$ UI.blur input]

    item <- Reactive.currentValue bValue
    case item of
        Data.Data item -> do
            void $ element input # set
                value
                (  extract (Photographer.unPhotographers item)
                ^. Photographer.name
                )
        _ -> return ()

    window <- askWindow

    liftIOLater $ Reactive.onChange bValue $ \s -> runUI window $ do
        case s of
            Data.NotAsked  -> void $ element content # set text "Not Asked"
            Data.Loading   -> void $ element content # set text "bobo"
            Data.Failure e -> do
                err <- string (show e)
                void $ element content # set children [] #+ [element err]
            Data.Data item -> do
                editing <- liftIO $ currentValue bEditing
                when (not editing) $ void $ do
                    element input # set
                        value
                        (  extract (Photographer.unPhotographers item)
                        ^. Photographer.name
                        )
                    element content # set children [] #+ [element input]

    let _elementTE = content
        _photographersTE =
            filterJust
                $   flap
                .   fmap setName
                <$> Data.toJust
                <$> bValue
                <@> UI.valueChange input

    return PhotographerEntry { .. }

setName :: Photographer.Photographers -> String -> Photographer.Photographers
setName photographers name = case Photographer.unPhotographers photographers of
    ListZipper.ListZipper ls x rs ->
        Photographer.Photographers
            $ ListZipper.ListZipper ls (x & Photographer.name .~ name) rs

-----------------------------------------------------------------------------
data PhotographersBox a = PhotographersBox
    { _elementPB   :: Element
    , _photographersPB :: !(Tidings (Maybe Photographer.Photographers))
    }

instance Widget (PhotographersBox a) where
    getElement = _elementPB

listBox
    :: Behavior (Data.Data String Photographer.Photographers)
    -> UI (PhotographersBox a)
listBox bitems = do
    _elementPB <- UI.div
    list       <- UI.select

    element _elementPB # sink (items list) bitems

    bb <- Reactive.stepper Nothing UI.never

    let _photographersPB =
            tidings (Data.toJust <$> bitems)
                $   selectPhotographeeF
                <$> (Data.toJust <$> bitems)
                <@> (filterJust (UI.selectionChange list))

    return PhotographersBox { .. }


selectPhotographeeF
    :: Maybe Photographer.Photographers
    -> Int
    -> Maybe Photographer.Photographers
selectPhotographeeF photographerss selected = case photographerss of
    Nothing            -> Nothing
    Just photographers -> asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex photographers'' -> if selected == thisIndex
            then Just (Photographer.Photographers photographers'')
            else Nothing
        )
        (Photographer.unPhotographers photographers)


mkPhotographers :: Photographer.Photographers -> [UI Element]
mkPhotographers photographers' = do
    let zipper = Photographer.unPhotographers photographers'
    let elems = ListZipper.iextend
            (\i photographers'' ->
                (i, zipper == photographers'', extract photographers'')
            )
            zipper
    fmap mkPhotographerListItem (ListZipper.toList elems)

mkPhotographerListItem :: (Int, Bool, Photographer.Photographer) -> UI Element
mkPhotographerListItem (thisIndex, isCenter, photographer) = do
    let name'  = photographer ^. Photographer.name
    let option = UI.option # set value (show thisIndex) # set text name'
    if isCenter then option # set UI.selected True else option


items list = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            element list # set children [] #+ (mkPhotographers item)
            return x # set children [] #+ [element list]

--------------------------------------------------------------------------------
data TabsBox a = TabsBox
    { _tabsE :: Element
    , _tabsPB :: !(Tidings (Maybe Tab.Tabs))
    }

instance Widget (TabsBox a) where
    getElement = _tabsE

tabsBox
    :: Behavior (Data.Data String Tab.Tabs)
    -> Behavior (Data.Data String Photographer.Photographers)
    -> UI (PhotographersBox b, TabsBox a)
tabsBox bitems bPhotographers = do
    _tabsE <- UI.div
    list   <- UI.select

    elem2  <- listBox bPhotographers

    element _tabsE # sink (tabItems list elem2) bitems

    bb <- Reactive.stepper Nothing UI.never

    let _tabsPB =
            tidings (Data.toJust <$> bitems)
                $   selectTabF
                <$> (Data.toJust <$> bitems)
                <@> (filterJust (UI.selectionChange list))

    return (elem2, TabsBox { .. })


selectTabF :: Maybe Tab.Tabs -> Int -> Maybe Tab.Tabs
selectTabF tabs selected = case tabs of
    Nothing   -> Nothing
    Just tabs -> asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex tabs'' ->
            if selected == thisIndex then Just (Tab.Tabs tabs'') else Nothing
        )
        (Tab.unTabs tabs)


mkTabs :: Tab.Tabs -> [UI Element]
mkTabs tabs' = do
    let zipper = Tab.unTabs tabs'
    let elems = ListZipper.iextend
            (\i tabs'' -> (i, zipper == tabs'', extract tabs''))
            zipper
    fmap mkTabListItem (ListZipper.toList elems)

mkTabListItem :: (Int, Bool, Tab.Tab) -> UI Element
mkTabListItem (thisIndex, isCenter, tab) = do
    let name'  = show tab --- FIX THIS translate
    let option = UI.option # set value (show thisIndex) # set text name'
    if isCenter then option # set UI.selected True else option


tabItems list photographers = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            case extract (Tab.unTabs item) of
                Tab.MainTab -> do
                    element list # set children [] #+ (mkTabs item)
                    return x
                        #  set children []
                        #+ [element list, element photographers]
                _ -> do
                    element list # set children [] #+ (mkTabs item)
                    return x # set children [] #+ [element list]

--------------------------------------------------------------------------------

setup :: AppEnv -> Window -> UI ()
setup env@Env {..} win = mdo
    _              <- return win # set title "FF"

    elem           <- entry bPhotographers
    (elem2, elem3) <- tabsBox bTabs bPhotographers

    let eElem = _photographersTE elem
    _ <- onEvent eElem $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WritePhographers item)

    let eElem2 = filterJust $ rumors $ _photographersPB elem2
    _ <- onEvent eElem2 $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WritePhographers item)

    let eElem3 = filterJust $ rumors $ _tabsPB elem3
    _ <- onEvent eElem3 $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteTabs item)


    void $ UI.getBody win #+ [element elem, element elem2, element elem3]

    _               <- liftIO $ runApp env setupStartMap
    _               <- liftIO $ runApp env startStartMap
    _               <- liftIO $ runApp env read

    messageReceiver <- liftIO $ forkIO $ runApp env (receiveMessages win)

    UI.on UI.disconnect win $ const $ liftIO $ do
        --HashMap ti list and kill all
        --HashMap ti list and kill all
        --HashMap ti list and kill all
        killThread messageReceiver
        return ()


type WithChan r m
    = ( MonadThrow m
      , MonadError AppError m
      , MonadReader r m
      , Has WatchManager r
      , Has HPhotographers r
      , Has MPhotographersFile r
      , Has HTabs r
      , Has MTabsFile r
      , Has (MStartMap m) r
      , Has MStopMap r
      , Has OutChan r
      , Has InChan r
      , MonadIO m
      , MonadCatch m
      )


setupStartMap :: forall  r m . WithChan r m => m ()
setupStartMap = do
    mStartMap <- unMStartMap <$> grab @(MStartMap m)
    startMap  <- takeMVar mStartMap

    let key          = "photographers"
    let watcher      = Watchers.photographersFile
    let newStartMap  = HashMap.insert key watcher startMap

    let key2         = "tabs"
    let watcher2     = Watchers.tabsFile
    let newStartMap2 = HashMap.insert key2 watcher2 newStartMap

    putMVar mStartMap newStartMap2


read :: forall  r m . WithChan r m => m ()
read = do
    inChan <- unInChan <$> grab @InChan
    liftIO $ Chan.writeChan inChan Message.ReadPhotographers
    liftIO $ Chan.writeChan inChan Message.ReadTabs

startStartMap :: forall  r m . WithChan r m => m ()
startStartMap = do
    inChan <- unInChan <$> grab @InChan
    liftIO $ Chan.writeChan inChan Message.StartPhotograpers
    liftIO $ Chan.writeChan inChan Message.StartTabs


receiveMessages :: forall  r m . WithChan r m => Window -> m ()
receiveMessages window = do
    outChan        <- grab @OutChan
    messages       <- liftIO $ Chan.getChanContents (unOutChan outChan)
    hPhotographers <- unHPhotographers <$> grab @HPhotographers
    forM_ messages $ \x -> do
        traceShowM x
        case x of
            Message.StopPhotographers -> do
                return ()

            Message.WritePhographers photographers -> do
                mPhotographersFile <-
                    unMPhotographersFile <$> grab @MPhotographersFile
                photographersFile <- takeMVar mPhotographersFile
                _ <- Photographer.writePhotographers photographersFile
                                                     photographers
                _ <- putMVar mPhotographersFile photographersFile
                _ <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
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
                traceShowM "wtf"
                mPhotographersFile <-
                    unMPhotographersFile <$> grab @MPhotographersFile
                photographersFile <- takeMVar mPhotographersFile
                traceShowM "wtf3"
                runIt window photographersFile mPhotographersFile
                    `E.catchError` (\e -> do
                                       hPhotographers <-
                                           unHPhotographers
                                               <$> grab @HPhotographers
                                       liftIO $ hPhotographers $ Data.Failure
                                           (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       traceShowM "wtf5"
                                       putMVar mPhotographersFile
                                               photographersFile
                                       traceShowM "wtf6"
                                   )
--------------------------------------------------------------------------------
            Message.StopTabs -> do
                return ()

            Message.WriteTabs tabs -> do
                mTabsFile <- unMTabsFile <$> grab @MTabsFile
                tabsFile  <- takeMVar mTabsFile
                _         <- Tab.writeTabs tabsFile tabs
                _         <- putMVar mTabsFile tabsFile
                _         <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartTabs -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "tabs"
                traceShowM (HashMap.keys startMap)
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadTabs -> do
                traceShowM "wtf"
                mTabsFile <- unMTabsFile <$> grab @MTabsFile
                tabsFile  <- takeMVar mTabsFile
                traceShowM "wtf3"
                runIt2 window tabsFile mTabsFile
                    `E.catchError` (\e -> do
                                       hTabs <- unHTabs <$> grab @HTabs
                                       liftIO $ hTabs $ Data.Failure (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       traceShowM "wtf5"
                                       putMVar mTabsFile tabsFile
                                       traceShowM "wtf6"
                                   )


runIt
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt window photographersFile mPhotographersFile = do
    photographers <- getPhotographers photographersFile
    traceShowM "wtf4"

    hPhotographers <- unHPhotographers <$> grab @HPhotographers

    liftIO $ hPhotographers $ Data.Data photographers
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mPhotographersFile photographersFile
    traceShowM "wtf2"


--type WithIOError m = MonadError AppError m
getPhotographers
    :: (MonadIO m, MonadCatch m, WithError m)
    => FilePath
    -> m Photographer.Photographers
getPhotographers fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )


runIt2
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt2 window tabsFile mTabsFile = do
    tabs  <- getTabs tabsFile
    hTabs <- unHTabs <$> grab @HTabs
    liftIO $ hTabs $ Data.Data tabs
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mTabsFile tabsFile


--type WithIOError m = MonadError AppError m
getTabs :: (MonadIO m, MonadCatch m, WithError m) => FilePath -> m Tab.Tabs
getTabs fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )


