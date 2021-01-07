{-# LANGUAGE RecursiveDo #-}
module Lib.Server
    ( setup
    )
where

import qualified Foreign.JavaScript            as JavaScript
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
                                                , HDumpDir(..)
                                                , MDumpFile(..)
                                                , HDoneshootingDir(..)
                                                , MDoneshootingFile(..)
                                                , HDagsdatoDir(..)
                                                , MDagsdatoFile(..)
                                                , HDagsdatoBackupDir(..)
                                                , MDagsdatoBackupFile(..)
                                                , HTabs(..)
                                                , MTabsFile(..)
                                                , HShootings(..)
                                                , MShootingsFile(..)

                                                , HSessions(..)
                                                , MSessionsFile(..)

                                                , HCameras(..)
                                                , MCamerasFile(..)
                                                , MStartMap(..)
                                                , WatchManager(..)
                                                , MStopMap(..)
                                                , OutChan(..)
                                                , InChan(..)
                                                , unMPhotographersFile
                                                , unHPhotographers
                                                , unMTabsFile
                                                , unHTabs
                                                , unMShootingsFile
                                                , unHShootings

                                                , unMSessionsFile
                                                , unHSessions
                                                , unMCamerasFile
                                                , unHCameras
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

import qualified Lib.Model.Doneshooting              as Doneshooting
import qualified Lib.Model.Camera              as Camera
import qualified Lib.Model.Dump                as Dump
import qualified Lib.Model.Tab                 as Tab
import qualified Lib.Model.Shooting            as Shooting
import qualified Lib.Model.Session            as Session
import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Model.Data                as Data
import qualified Lib.Model.Dagsdato            as Dagsdato
import qualified Lib.Model.DagsdatoBackup            as DagsdatoBackup


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
data ShootingsBox a = ShootingsBox
    { _elementShootingPB   :: Element
    , _shootingsPB :: !(Tidings (Maybe Shooting.Shootings))
    }

instance Widget (ShootingsBox a) where
    getElement = _elementShootingPB

listBoxShootings
    :: Behavior (Data.Data String Shooting.Shootings) -> UI (ShootingsBox a)
listBoxShootings bitems = do
    _elementShootingPB <- UI.div
    list               <- UI.select

    element _elementShootingPB # sink (itemsShooting list) bitems


    let _shootingsPB =
            tidings (Data.toJust <$> bitems)
                $   selectShootingF
                <$> (Data.toJust <$> bitems)
                <@> (filterJust (UI.selectionChange list))

    return ShootingsBox { .. }


selectShootingF :: Maybe Shooting.Shootings -> Int -> Maybe Shooting.Shootings
selectShootingF shootings selected = case shootings of
    Nothing        -> Nothing
    Just shootings -> asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex shootings'' -> if selected == thisIndex
            then Just (Shooting.Shootings shootings'')
            else Nothing
        )
        (Shooting.unShootings shootings)


mkShootings :: Shooting.Shootings -> [UI Element]
mkShootings shootings' = do
    let zipper = Shooting.unShootings shootings'
    let elems = ListZipper.iextend
            (\i shootings'' -> (i, zipper == shootings'', extract shootings''))
            zipper
    fmap mkShootingListItem (ListZipper.toList elems)

mkShootingListItem :: (Int, Bool, Shooting.Shooting) -> UI Element
mkShootingListItem (thisIndex, isCenter, shooting) = do
    let name'  = show shooting -- change 
    let option = UI.option # set value (show thisIndex) # set text name'
    if isCenter then option # set UI.selected True else option


itemsShooting list = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            element list # set children [] #+ (mkShootings item)
            return x # set children [] #+ [element list]

--------------------------------------------------------------------------------
data SessionsBox a = SessionsBox
    { _elementSessionsPB   :: Element
    , _sessionsPB :: !(Tidings (Maybe Session.Sessions))
    }

instance Widget (SessionsBox a) where
    getElement = _elementSessionsPB

listBoxSessions
    :: Behavior (Data.Data String Session.Sessions) -> UI (SessionsBox a)
listBoxSessions bitems = do
    _elementSessionsPB <- UI.div
    list               <- UI.select

    element _elementSessionsPB # sink (itemsSession list) bitems


    let _sessionsPB =
            tidings (Data.toJust <$> bitems)
                $   selectSessionF
                <$> (Data.toJust <$> bitems)
                <@> (filterJust (UI.selectionChange list))

    return SessionsBox { .. }


selectSessionF :: Maybe Session.Sessions -> Int -> Maybe Session.Sessions
selectSessionF sessions selected = case sessions of
    Nothing        -> Nothing
    Just sessions -> asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex sessions'' -> if selected == thisIndex
            then Just (Session.Sessions sessions'')
            else Nothing
        )
        (Session.unSessions sessions)


mkSessions :: Session.Sessions -> [UI Element]
mkSessions sessions' = do
    let zipper = Session.unSessions sessions'
    let elems = ListZipper.iextend
            (\i sessions'' -> (i, zipper == sessions'', extract sessions''))
            zipper
    fmap mkSessionListItem (ListZipper.toList elems)


mkSessionListItem :: (Int, Bool, Session.Session) -> UI Element
mkSessionListItem (thisIndex, isCenter, session) = do
    let name'  = show session -- change 
    let option = UI.option # set value (show thisIndex) # set text name'
    if isCenter then option # set UI.selected True else option


itemsSession list = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            element list # set children [] #+ (mkSessions item)
            return x # set children [] #+ [element list]

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
data CamerasBox a = CamerasBox
    { _elementCamerasPB   :: Element
    , _camerasPB :: !(Tidings (Maybe Camera.Cameras))
    }

instance Widget (CamerasBox a) where
    getElement = _elementCamerasPB

listBoxCameras
    :: Behavior (Data.Data String Camera.Cameras) -> UI (CamerasBox a)
listBoxCameras bitems = do
    _elementCamerasPB <- UI.div
    list               <- UI.select

    element _elementCamerasPB # sink (itemsCamera list) bitems


    let _camerasPB =
            tidings (Data.toJust <$> bitems)
                $   selectCameraF
                <$> (Data.toJust <$> bitems)
                <@> (filterJust (UI.selectionChange list))

    return CamerasBox { .. }


selectCameraF :: Maybe Camera.Cameras -> Int -> Maybe Camera.Cameras
selectCameraF cameras selected = case cameras of
    Nothing        -> Nothing
    Just cameras -> asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex cameras'' -> if selected == thisIndex
            then Just (Camera.Cameras cameras'')
            else Nothing
        )
        (Camera.unCameras cameras)


mkCameras :: Camera.Cameras -> [UI Element]
mkCameras cameras' = do
    let zipper = Camera.unCameras cameras'
    let elems = ListZipper.iextend
            (\i cameras'' -> (i, zipper == cameras'', extract cameras''))
            zipper
    fmap mkCameraListItem (ListZipper.toList elems)

mkCameraListItem :: (Int, Bool, Camera.Camera) -> UI Element
mkCameraListItem (thisIndex, isCenter, camera) = do
    let name'  = show camera -- change 
    let option = UI.option # set value (show thisIndex) # set text name'
    if isCenter then option # set UI.selected True else option


itemsCamera list = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            element list # set children [] #+ (mkCameras item)
            return x # set children [] #+ [element list]

--------------------------------------------------------------------------------

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
data DumpBox a = DumpBox
    { _elementDumpPB   :: Element
    , _dumpPB :: !(Tidings (Maybe Dump.Dump))
    }

instance Widget (DumpBox a) where
    getElement = _elementDumpPB



listBoxDump :: Behavior (Data.Data String Dump.Dump) -> UI (DumpBox a)
listBoxDump bitems = do
    _elementDumpPB <- UI.div

    selector       <- UI.button

    element _elementDumpPB # sink (dumpItem selector) bitems


    let _dumpPB = -- NOT SURE ABOUT ANY OF THIS SHITE.
            tidings (Data.toJust <$> bitems)
                $  (Data.toJust <$> bitems)
                <@ UI.click selector

    return DumpBox { .. }


dumpItem folderPicker = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            element folderPicker # set children [] #+ [mkFolderPicker item]
            return x # set children [] #+ [element folderPicker]

mkFolderPicker :: Dump.Dump -> UI Element
mkFolderPicker dump = do
    let name' = Dump.unDump dump
    UI.p # set text name'


--------------------------------------------------------------------------------
data DoneshootingBox a = DoneshootingBox
    { _elementDoneshootingPB   :: Element
    , _doneshootingPB :: !(Tidings (Maybe Doneshooting.Doneshooting))
    }

instance Widget (DoneshootingBox a) where
    getElement = _elementDoneshootingPB



listBoxDoneshooting
    :: Behavior (Data.Data String Doneshooting.Doneshooting) -> UI (DoneshootingBox a)
listBoxDoneshooting bitems = do
    _elementDoneshootingPB <- UI.div

    selector           <- UI.button

    element _elementDoneshootingPB # sink (doneshootingItem selector) bitems


    let _doneshootingPB = -- NOT SURE ABOUT ANY OF THIS SHITE.
            tidings (Data.toJust <$> bitems)
                $  (Data.toJust <$> bitems)
                <@ UI.click selector

    return DoneshootingBox { .. }


doneshootingItem folderPicker = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            element folderPicker # set children [] #+ [mkFolderPicker3 item]
            return x # set children [] #+ [element folderPicker]

mkFolderPicker3 :: Doneshooting.Doneshooting -> UI Element
mkFolderPicker3 doneshooting = do
    let name' = Doneshooting.unDoneshooting doneshooting
    UI.p # set text name'


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
data DagsdatoBox a = DagsdatoBox
    { _elementDagsdatoPB   :: Element
    , _dagsdatoPB :: !(Tidings (Maybe Dagsdato.Dagsdato))
    }

instance Widget (DagsdatoBox a) where
    getElement = _elementDagsdatoPB



listBoxDagsdato
    :: Behavior (Data.Data String Dagsdato.Dagsdato) -> UI (DagsdatoBox a)
listBoxDagsdato bitems = do
    _elementDagsdatoPB <- UI.div

    selector           <- UI.button

    element _elementDagsdatoPB # sink (dagsdatoItem selector) bitems


    let _dagsdatoPB = -- NOT SURE ABOUT ANY OF THIS SHITE.
            tidings (Data.toJust <$> bitems)
                $  (Data.toJust <$> bitems)
                <@ UI.click selector

    return DagsdatoBox { .. }


dagsdatoItem folderPicker = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            element folderPicker # set children [] #+ [mkFolderPicker2 item]
            return x # set children [] #+ [element folderPicker]

mkFolderPicker2 :: Dagsdato.Dagsdato -> UI Element
mkFolderPicker2 dagsdato = do
    let name' = Dagsdato.unDagsdato dagsdato
    UI.p # set text name'


--------------------------------------------------------------------------------
data DagsdatoBackupBox a = DagsdatoBackupBox
    { _elementDagsdatoBackupPB   :: Element
    , _dagsdatoBackupPB :: !(Tidings (Maybe DagsdatoBackup.DagsdatoBackup))
    }

instance Widget (DagsdatoBackupBox a) where
    getElement = _elementDagsdatoBackupPB



listBoxDagsdatoBackup
    :: Behavior (Data.Data String DagsdatoBackup.DagsdatoBackup) -> UI (DagsdatoBackupBox a)
listBoxDagsdatoBackup bitems = do
    _elementDagsdatoBackupPB <- UI.div

    selector           <- UI.button

    element _elementDagsdatoBackupPB # sink (dagsdatoBackupItem selector) bitems


    let _dagsdatoBackupPB = -- NOT SURE ABOUT ANY OF THIS SHITE.
            tidings (Data.toJust <$> bitems)
                $  (Data.toJust <$> bitems)
                <@ UI.click selector

    return DagsdatoBackupBox { .. }



dagsdatoBackupItem folderPicker = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            element folderPicker # set children [] #+ [mkFolderPicker4 item]
            return x # set children [] #+ [element folderPicker]

mkFolderPicker4 :: DagsdatoBackup.DagsdatoBackup -> UI Element
mkFolderPicker4 dagsdatoBackup = do
    let name' = DagsdatoBackup.unDagsdatoBackup dagsdatoBackup
    UI.p # set text name'


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
    -> Behavior (Data.Data String Shooting.Shootings)
    -> Behavior (Data.Data String Dump.Dump)
    -> Behavior (Data.Data String Dagsdato.Dagsdato)
    -> Behavior (Data.Data String Camera.Cameras)
    -> Behavior (Data.Data String Doneshooting.Doneshooting)
    -> Behavior (Data.Data String DagsdatoBackup.DagsdatoBackup)
    -> Behavior (Data.Data String Session.Sessions)
    -> UI
           ( PhotographersBox b
           , ShootingsBox c
           , DumpBox d
           , DagsdatoBox e
           , CamerasBox f
           , DoneshootingBox f
           , DagsdatoBackupBox e
           , SessionsBox g
           , TabsBox a
           )
tabsBox bTabs bPhotographers bShootings bDump bDagsdato bCameras bDoneshooting bDagsdatoBackup bSessions= do
    _tabsE            <- UI.div
    list              <- UI.select

    elemPhotographers <- listBox bPhotographers
    elemShootings     <- listBoxShootings bShootings
    elemDump          <- listBoxDump bDump
    elemDagsdato      <- listBoxDagsdato bDagsdato
    elemCameras       <- listBoxCameras bCameras
    elemDoneshooting <- listBoxDoneshooting bDoneshooting
    elemDagsdatoBackup <- listBoxDagsdatoBackup bDagsdatoBackup
    elemSessions <- listBoxSessions bSessions

    element _tabsE
        # sink
              (tabItems list
                        elemPhotographers
                        elemShootings
                        elemDump
                        elemDagsdato
                        elemCameras
                        elemDoneshooting
                        elemDagsdatoBackup
                        elemSessions
              )
              bTabs


    let _tabsPB =
            tidings (Data.toJust <$> bTabs)
                $   selectTabF
                <$> (Data.toJust <$> bTabs)
                <@> (filterJust (UI.selectionChange list))

    return
        ( elemPhotographers
        , elemShootings
        , elemDump
        , elemDagsdato
        , elemCameras
        , elemDoneshooting
        , elemDagsdatoBackup
        , elemSessions
        , TabsBox { .. }
        )


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


tabItems list photographers shootings dump dagsdato cameras doneshooting dagsdatoBackup sessions = mkWriteAttr $ \i x ->
    void $ do
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
                            #  set children [] -- THIS IS DANGEROUS?
                            #+ [element list]

                    Tab.ShootingsTab -> do
                        element list # set children [] #+ (mkTabs item)
                        return x
                            #  set children []
                            #+ [element list, element shootings]

                    Tab.SessionsTab -> do
                        element list # set children [] #+ (mkTabs item)
                        return x
                            #  set children []
                            #+ [element list, element sessions]

                    Tab.CamerasTab -> do
                        element list # set children [] #+ (mkTabs item)
                        return x
                            #  set children []
                            #+ [element list, element cameras]

                    Tab.DumpTab -> do
                        element list # set children [] #+ (mkTabs item)
                        return x
                            #  set children []
                            #+ [element list, element dump]

                    Tab.DoneshootingTab -> do
                        element list # set children [] #+ (mkTabs item)
                        return x
                            #  set children []
                            #+ [element list, element doneshooting]

                    Tab.DagsdatoTab -> do
                        element list # set children [] #+ (mkTabs item)
                        return x
                            #  set children []
                            #+ [element list, element dagsdato]

                    Tab.DagsdatoBackupTab -> do
                        element list # set children [] #+ (mkTabs item)
                        return x
                            #  set children []
                            #+ [element list, element dagsdatoBackup]

                    Tab.PhotographersTab -> do
                        element list # set children [] #+ (mkTabs item)
                        return x
                            #  set children []
                            #+ [element list, element photographers]

                    _ -> do
                        element list # set children [] #+ (mkTabs item)
                        return x # set children [] #+ [element list]

--------------------------------------------------------------------------------
example :: [String] -> JavaScript.JSObject -> JSFunction ()
example options callback = ffi
    "require('electron').remote.dialog.showOpenDialog({properties: %2}).then(result => %1(result.filePaths[0]))"
    callback
    options

setup :: AppEnv -> Window -> UI ()
setup env@Env {..} win = mdo
    _    <- return win # set title "FF"

    elem <- entry bPhotographers
    (elemPhotographers, elemShootings, elemDump, elemDagsdato, elemCameras, elemDoneshooting, elemDagsdatoBackup, elemSessions,elem3) <- tabsBox
        bTabs
        bPhotographers
        bShootings
        bDumpDir
        bDagsdatoDir
        bCameras
        bDoneshootingDir
        bDagsdatoBackupDir
        bSessions


    let eElemPhotographers =
            filterJust $ rumors $ _photographersPB elemPhotographers
    _ <- onEvent eElemPhotographers $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WritePhographers item)

    let eElemShootings = filterJust $ rumors $ _shootingsPB elemShootings
    _ <- onEvent eElemShootings $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteShootings item)

    let eElemSessions = filterJust $ rumors $ _sessionsPB elemSessions
    _ <- onEvent eElemSessions $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteSessions item)




    let fx =
            (\folder -> when (folder /= "") $ liftIO $ void $ Chan.writeChan
                (unInChan inChan)
                (Message.WriteDump (Dump.Dump folder))
            ) :: FilePath -> IO ()

    callback <- ffiExport fx
    let eElemDump = filterJust $ rumors $ _dumpPB elemDump
    _ <- onEvent eElemDump $ \item -> do
        runFunction $ example ["openDirectory"] callback

    ------------------------------------------------------------------------------
    let fx2 =
            (\folder -> when (folder /= "") $ liftIO $ void $ Chan.writeChan
                (unInChan inChan)
                (Message.WriteDagsdato (Dagsdato.Dagsdato folder))
            ) :: FilePath -> IO ()

    callback2 <- ffiExport fx2
    let eElemDagsdato = filterJust $ rumors $ _dagsdatoPB elemDagsdato
    _ <- onEvent eElemDagsdato $ \item -> do
        runFunction $ example ["openDirectory"] callback2

    ------------------------------------------------------------------------------
    let fx3 =
            (\folder -> when (folder /= "") $ liftIO $ void $ Chan.writeChan
                (unInChan inChan)
                (Message.WriteDoneshooting (Doneshooting.Doneshooting folder))
            ) :: FilePath -> IO ()

    callback3 <- ffiExport fx3
    let eElemDoneshooting = filterJust $ rumors $ _doneshootingPB elemDoneshooting
    _ <- onEvent eElemDoneshooting $ \item -> do
        runFunction $ example ["openDirectory"] callback3

    ------------------------------------------------------------------------------
    let fx4 =
            (\folder -> when (folder /= "") $ liftIO $ void $ Chan.writeChan
                (unInChan inChan)
                (Message.WriteDagsdatoBackup (DagsdatoBackup.DagsdatoBackup folder))
            ) :: FilePath -> IO ()

    callback4 <- ffiExport fx4
    let eElemDagsdatoBackup = filterJust $ rumors $ _dagsdatoBackupPB elemDagsdatoBackup
    _ <- onEvent eElemDagsdatoBackup $ \item -> do
        runFunction $ example ["openDirectory"] callback4


    let eElem3 = filterJust $ rumors $ _tabsPB elem3
    _ <- onEvent eElem3 $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteTabs item)

    let eElemCameras = filterJust $ rumors $ _camerasPB elemCameras
    _ <- onEvent eElemCameras $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteCameras item)

    void $ UI.getBody win #+ [element elem3]

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
      , Has HShootings r
      , Has MShootingsFile r
      , Has HSessions r
      , Has MSessionsFile r
      , Has HCameras r
      , Has MCamerasFile r
      , Has HDumpDir r
      , Has MDumpFile r
      , Has HDagsdatoDir r
      , Has MDagsdatoFile r
      , Has HDagsdatoBackupDir r
      , Has MDagsdatoBackupFile r
      , Has HDoneshootingDir r
      , Has MDoneshootingFile r
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

    let key3         = "shootings"
    let watcher3     = Watchers.shootingsFile
    let newStartMap3 = HashMap.insert key3 watcher3 newStartMap2

    let key4         = "dump"
    let watcher4     = Watchers.dumpFile
    let newStartMap4 = HashMap.insert key4 watcher4 newStartMap3

    let key5         = "dagsdato"
    let watcher5     = Watchers.dagsdatoFile
    let newStartMap5 = HashMap.insert key5 watcher5 newStartMap4

    let key6         = "cameras"
    let watcher6     = Watchers.camerasFile
    let newStartMap6 = HashMap.insert key6 watcher6 newStartMap5

    let key7         = "doneshooting"
    let watcher7     = Watchers.doneshootingFile
    let newStartMap7 = HashMap.insert key7 watcher7 newStartMap6


    let key8 = "dagsdatoBackup"
    let watcher8     = Watchers.dagsdatoFile
    let newStartMap8 = HashMap.insert key8 watcher8 newStartMap7

    let key9 = "sessions"
    let watcher9 = Watchers.sessionsFile
    let newStartMap9 = HashMap.insert key9 watcher9 newStartMap8

    putMVar mStartMap newStartMap9


read :: forall  r m . WithChan r m => m ()
read = do
    inChan <- unInChan <$> grab @InChan
    liftIO $ Chan.writeChan inChan Message.ReadPhotographers
    liftIO $ Chan.writeChan inChan Message.ReadTabs
    liftIO $ Chan.writeChan inChan Message.ReadShootings
    liftIO $ Chan.writeChan inChan Message.ReadSessions
    liftIO $ Chan.writeChan inChan Message.ReadDump
    liftIO $ Chan.writeChan inChan Message.ReadDagsdato
    liftIO $ Chan.writeChan inChan Message.ReadCameras
    liftIO $ Chan.writeChan inChan Message.ReadDoneshooting
    liftIO $ Chan.writeChan inChan Message.ReadDagsdatoBackup

startStartMap :: forall  r m . WithChan r m => m ()
startStartMap = do
    inChan <- unInChan <$> grab @InChan
    liftIO $ Chan.writeChan inChan Message.StartPhotograpers
    liftIO $ Chan.writeChan inChan Message.StartTabs
    liftIO $ Chan.writeChan inChan Message.StartShootings
    liftIO $ Chan.writeChan inChan Message.StartSessions
    liftIO $ Chan.writeChan inChan Message.StartDump
    liftIO $ Chan.writeChan inChan Message.StartDagsdato
    liftIO $ Chan.writeChan inChan Message.StartCameras
    liftIO $ Chan.writeChan inChan Message.StartDoneshooting
    liftIO $ Chan.writeChan inChan Message.StartDagsdatoBackup


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
--------------------------------------------------------------------------------
            Message.StopShootings -> do
                return ()

            Message.WriteShootings shootings -> do
                mShootingsFile <- unMShootingsFile <$> grab @MShootingsFile
                shootingsFile  <- takeMVar mShootingsFile
                _ <- Shooting.writeShootings shootingsFile shootings
                _              <- putMVar mShootingsFile shootingsFile
                _              <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartShootings -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "shootings"
                traceShowM (HashMap.keys startMap)
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadShootings -> do
                traceShowM "wtf"
                mShootingsFile <- unMShootingsFile <$> grab @MShootingsFile
                shootingsFile  <- takeMVar mShootingsFile
                traceShowM "wtf3"
                runIt3 window shootingsFile mShootingsFile
                    `E.catchError` (\e -> do
                                       hShootings <-
                                           unHShootings <$> grab @HShootings
                                       liftIO $ hShootings $ Data.Failure
                                           (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       traceShowM "wtf5"
                                       putMVar mShootingsFile shootingsFile
                                       traceShowM "wtf6"
                                   )

--------------------------------------------------------------------------------
            Message.StopDump -> do
                return ()

            Message.WriteDump dump -> do
                mDumpFile <- unMDumpFile <$> grab @MDumpFile
                dumpFile  <- takeMVar mDumpFile
                _         <- Dump.writeDump dumpFile dump
                _         <- putMVar mDumpFile dumpFile
                _         <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartDump -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "dump"
                traceShowM (HashMap.keys startMap)
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadDump -> do
                traceShowM "wtf"
                mDumpFile <- unMDumpFile <$> grab @MDumpFile
                dumpFile  <- takeMVar mDumpFile
                traceShowM "wtf3"
                runIt4 window dumpFile mDumpFile
                    `E.catchError` (\e -> do
                                       hDumpDir <- unHDumpDir <$> grab @HDumpDir
                                       liftIO $ hDumpDir $ Data.Failure (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       traceShowM "wtf5"
                                       putMVar mDumpFile dumpFile
                                       traceShowM "wtf6"
                                   )
--------------------------------------------------------------------------------
            Message.StopDagsdato -> do
                return ()

            Message.WriteDagsdato dagsdato -> do
                mDagsdatoFile <- unMDagsdatoFile <$> grab @MDagsdatoFile
                dagsdatoFile  <- takeMVar mDagsdatoFile
                _             <- Dagsdato.writeDagsdato dagsdatoFile dagsdato
                _             <- putMVar mDagsdatoFile dagsdatoFile
                _             <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartDagsdato -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "dagsdato"
                traceShowM (HashMap.keys startMap)
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadDagsdato -> do
                traceShowM "wtf"
                mDagsdatoFile <- unMDagsdatoFile <$> grab @MDagsdatoFile
                dagsdatoFile  <- takeMVar mDagsdatoFile
                traceShowM "wtf3"
                runIt5 window dagsdatoFile mDagsdatoFile
                    `E.catchError` (\e -> do
                                       hDagsdatoDir <-
                                           unHDagsdatoDir <$> grab @HDagsdatoDir
                                       liftIO $ hDagsdatoDir $ Data.Failure
                                           (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       traceShowM "wtf5"
                                       putMVar mDagsdatoFile dagsdatoFile
                                       traceShowM "wtf6"
                                   )

--------------------------------------------------------------------------------
            Message.StopCameras -> do
                return ()

            Message.WriteCameras cameras -> do
                mCamerasFile <- unMCamerasFile <$> grab @MCamerasFile
                camerasFile  <- takeMVar mCamerasFile
                _            <- Camera.writeCameras camerasFile cameras
                _            <- putMVar mCamerasFile camerasFile
                _            <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartCameras -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "cameras"
                traceShowM (HashMap.keys startMap)
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadCameras -> do
                traceShowM "wtf"
                mCamerasFile <- unMCamerasFile <$> grab @MCamerasFile
                camerasFile  <- takeMVar mCamerasFile
                traceShowM "wtf3"
                runIt6 window camerasFile mCamerasFile
                    `E.catchError` (\e -> do
                                       hCameras <- unHCameras <$> grab @HCameras
                                       liftIO $ hCameras $ Data.Failure (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       traceShowM "wtf5"
                                       putMVar mCamerasFile camerasFile
                                       traceShowM "wtf6"
                                   )

--------------------------------------------------------------------------------
            Message.StopDoneshooting -> do
                return ()

            Message.WriteDoneshooting doneshooting -> do
                mDoneshootingFile <- unMDoneshootingFile <$> grab @MDoneshootingFile
                doneshootingFile  <- takeMVar mDoneshootingFile
                _             <- Doneshooting.writeDoneshooting doneshootingFile doneshooting
                _             <- putMVar mDoneshootingFile doneshootingFile
                _             <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartDoneshooting -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "doneshooting"
                traceShowM (HashMap.keys startMap)
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadDoneshooting -> do
                traceShowM "wtf"
                mDoneshootingFile <- unMDoneshootingFile <$> grab @MDoneshootingFile
                doneshootingFile  <- takeMVar mDoneshootingFile
                traceShowM "wtf3"
                runIt7 window doneshootingFile mDoneshootingFile
                    `E.catchError` (\e -> do
                                       hDoneshootingDir <-
                                           unHDoneshootingDir <$> grab @HDoneshootingDir
                                       liftIO $ hDoneshootingDir $ Data.Failure
                                           (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       traceShowM "wtf5"
                                       putMVar mDoneshootingFile doneshootingFile
                                       traceShowM "wtf6"
                                   )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
            Message.StopDagsdatoBackup -> do
                return ()

            Message.WriteDagsdatoBackup dagsdatoBackup -> do
                mDagsdatoBackupFile <- unMDagsdatoBackupFile <$> grab @MDagsdatoBackupFile
                dagsdatoBackupFile  <- takeMVar mDagsdatoBackupFile
                _             <- DagsdatoBackup.writeDagsdatoBackup dagsdatoBackupFile dagsdatoBackup
                _             <- putMVar mDagsdatoBackupFile dagsdatoBackupFile
                _             <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartDagsdatoBackup -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "dagsdatoBackup"
                traceShowM (HashMap.keys startMap)
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadDagsdatoBackup -> do
                traceShowM "wtf"
                mDagsdatoBackupFile <- unMDagsdatoBackupFile <$> grab @MDagsdatoBackupFile
                dagsdatoBackupFile  <- takeMVar mDagsdatoBackupFile
                traceShowM "wtf3"
                runIt8 window dagsdatoBackupFile mDagsdatoBackupFile
                    `E.catchError` (\e -> do
                                       hDagsdatoBackupDir <-
                                           unHDagsdatoBackupDir <$> grab @HDagsdatoBackupDir
                                       liftIO $ hDagsdatoBackupDir $ Data.Failure
                                           (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       traceShowM "wtf5"
                                       putMVar mDagsdatoBackupFile dagsdatoBackupFile
                                       traceShowM "wtf6"
                                   )
--------------------------------------------------------------------------------
            Message.StopSessions -> do
                return ()

            Message.WriteSessions sessions -> do
                mSessionsFile <- unMSessionsFile <$> grab @MSessionsFile
                sessionsFile  <- takeMVar mSessionsFile
                _            <- Session.writeSessions sessionsFile sessions
                _            <- putMVar mSessionsFile sessionsFile
                _            <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartSessions-> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "sessions"
                traceShowM (HashMap.keys startMap)
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadSessions -> do
                traceShowM "wtf"
                mSessionsFile <- unMSessionsFile <$> grab @MSessionsFile
                sessionsFile  <- takeMVar mSessionsFile
                traceShowM "wtf3"
                runIt9 window sessionsFile mSessionsFile
                    `E.catchError` (\e -> do
                                       hSessions <- unHSessions <$> grab @HSessions
                                       liftIO $ hSessions $ Data.Failure (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       traceShowM "wtf5"
                                       putMVar mSessionsFile sessionsFile
                                       traceShowM "wtf6"
                                   )


--------------------------------------------------------------------------------


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


runIt3
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt3 window shootingsFile mShootingsFile = do
    shootings  <- getShootings shootingsFile
    hShootings <- unHShootings <$> grab @HShootings
    liftIO $ hShootings $ Data.Data shootings
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mShootingsFile shootingsFile


--type WithIOError m = MonadError AppError m
getShootings
    :: (MonadIO m, MonadCatch m, WithError m)
    => FilePath
    -> m Shooting.Shootings
getShootings fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )

runIt4
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt4 window dumpFile mDumpFile = do
    dump     <- getDump dumpFile
    hDumpDir <- unHDumpDir <$> grab @HDumpDir
    liftIO $ hDumpDir $ Data.Data dump
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mDumpFile dumpFile


--type WithIOError m = MonadError AppError m
getDump :: (MonadIO m, MonadCatch m, WithError m) => FilePath -> m Dump.Dump
getDump fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )

runIt5
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt5 window dagsdatoFile mDagsdatoFile = do
    dagsdato     <- getDagsdato dagsdatoFile
    hDagsdatoDir <- unHDagsdatoDir <$> grab @HDagsdatoDir
    liftIO $ hDagsdatoDir $ Data.Data dagsdato
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mDagsdatoFile dagsdatoFile


--type WithIOError m = MonadError AppError m
getDagsdato
    :: (MonadIO m, MonadCatch m, WithError m) => FilePath -> m Dagsdato.Dagsdato
getDagsdato fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )

runIt6
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt6 window camerasFile mCamerasFile = do
    cameras     <- getCameras camerasFile
    hCameras <- unHCameras <$> grab @HCameras
    liftIO $ hCameras $ Data.Data cameras
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mCamerasFile camerasFile


--type WithIOError m = MonadError AppError m
getCameras
    :: (MonadIO m, MonadCatch m, WithError m) => FilePath -> m Camera.Cameras
getCameras fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )


runIt7
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt7 window doneshootingFile mDoneshootingFile = do
    doneshooting     <- getDoneshooting doneshootingFile
    hDoneshootingDir <- unHDoneshootingDir <$> grab @HDoneshootingDir
    liftIO $ hDoneshootingDir $ Data.Data doneshooting
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mDoneshootingFile doneshootingFile


--type WithIOError m = MonadError AppError m
getDoneshooting
    :: (MonadIO m, MonadCatch m, WithError m) => FilePath -> m Doneshooting.Doneshooting
getDoneshooting fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )


runIt8
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt8 window dagsdatoBackupFile mDagsdatoBackupFile = do
    dagsdatoBackup     <- getDagsdatoBackup dagsdatoBackupFile
    hDagsdatoBackupDir <- unHDagsdatoBackupDir <$> grab @HDagsdatoBackupDir
    liftIO $ hDagsdatoBackupDir $ Data.Data dagsdatoBackup
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mDagsdatoBackupFile dagsdatoBackupFile


--type WithIOError m = MonadError AppError m
getDagsdatoBackup
    :: (MonadIO m, MonadCatch m, WithError m) => FilePath -> m DagsdatoBackup.DagsdatoBackup
getDagsdatoBackup fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )

runIt9
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt9 window sessionsFile mSessionsFile = do
    sessions     <- getSessions sessionsFile
    hSessions <- unHSessions <$> grab @HSessions
    liftIO $ hSessions $ Data.Data sessions
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mSessionsFile sessionsFile


--type WithIOError m = MonadError AppError m
getSessions
    :: (MonadIO m, MonadCatch m, WithError m) => FilePath -> m Session.Sessions
getSessions fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
