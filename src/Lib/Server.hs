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
    , _photographersTE    :: !(Event Photographer.Photographers)
    }

instance Widget PhotographerEntry where
    getElement = _elementTE

entry
    :: Behavior (Model.Data String Photographer.Photographers)
    -> UI PhotographerEntry
entry bValue = do
    content  <- UI.div
    input    <- UI.input
    bEditing <- stepper False $ and <$> unions
        [True <$ UI.focus input, False <$ UI.blur input]

    item <- Reactive.currentValue bValue
    case item of
        Model.Data item -> do
            void $ element input # set
                value
                (  extract (Photographer.unPhotographers item)
                ^. Photographer.name
                )
        _ -> return ()

    window <- askWindow

    liftIOLater $ Reactive.onChange bValue $ \s -> runUI window $ do
        case s of
            Model.NotAsked     -> void $ element content # set text "Not Asked"
            Model.Loading      -> void $ element content # set text "bobo"
            Model.Failure e    -> do
                err <- string (show e)
                void $ element content # set children [] #+ [element err]
            Model.Data    item -> do
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
                <$> Model.toJust
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
    :: Behavior (Model.Data String Photographer.Photographers)
    -> UI (PhotographersBox a)
listBox bitems = do
    _elementPB <- UI.div
    list       <- UI.select

    element _elementPB # sink (items list) bitems

    bb <- Reactive.stepper Nothing UI.never

    let _photographersPB =
            tidings (Model.toJust <$> bitems)
                $   selectPhotographeeF
                <$> (Model.toJust <$> bitems)
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
        Model.NotAsked     -> return x # set text "Not Asked"
        Model.Loading      -> return x # set text "bobo"
        Model.Failure e    -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Model.Data    item -> do
            element list # set children [] #+ (mkPhotographers item)
            return x # set children [] #+ [element list]

--------------------------------------------------------------------------------

setup :: AppEnv -> Window -> UI ()
setup env@Env {..} win = mdo
    _       <- return win # set title "FF"
    content <- UI.p # set text "bob"
    elem    <- entry bPhotographers
    elem2   <- listBox bPhotographers

    let eElem = _photographersTE elem
    _ <- onEvent eElem $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WritePhographers item)

    let eElem2 = filterJust $ rumors $ _photographersPB elem2
    _ <- onEvent eElem2 $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WritePhographers item)

    void $ UI.getBody win #+ [element elem, element elem2]

    _               <- liftIO $ runApp env setupStartMap
    _               <- liftIO $ runApp env startStartMap
    _               <- liftIO $ runApp env read

    messageReceiver <- liftIO $ forkIO $ runApp env (receiveMessages win)


    UI.on UI.disconnect win $ const $ liftIO $ do
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
    let key         = "photographers"
    let watcher     = Watchers.photographersFile
    let newStartMap = HashMap.insert key watcher startMap
    putMVar mStartMap newStartMap


read :: forall  r m . WithChan r m => m ()
read = do
    inChan <- unInChan <$> grab @InChan
    liftIO $ Chan.writeChan inChan Message.ReadPhotographers

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
                mPhotographersFile <- unMPhotographersFile <$> grab @MPhotographersFile
                photographersFile <- takeMVar mPhotographersFile
                traceShowM "wtf3"
                runIt window photographersFile mPhotographersFile `E.catchError` (\e -> do
                    hPhotographers <- unHPhotographers <$> grab @HPhotographers
                    liftIO $ hPhotographers $ Model.Failure (show e)
                    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                    traceShowM "wtf5"
                    putMVar mPhotographersFile photographersFile
                    traceShowM "wtf6"
                                                            )

runIt :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt window photographersFile mPhotographersFile = do
    photographers     <- getPhotographers photographersFile
    traceShowM "wtf4"

    hPhotographers <- unHPhotographers <$> grab @HPhotographers

    liftIO $ hPhotographers $ Model.Data photographers
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mPhotographersFile photographersFile
    traceShowM "wtf2"


--type WithIOError m = MonadError AppError m
getPhotographers
    :: (MonadIO m, MonadCatch m, WithError m) => FilePath -> m Photographer.Photographers
getPhotographers fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e then
                                E.throwError (InternalError $ ServerError (show e))
                            else
                                E.throwError (InternalError $ WTF)
                        )


