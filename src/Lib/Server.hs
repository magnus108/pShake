{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
module Lib.Server
    ( setup
    )
where


import           Safe                             (atMay)
import Data.String.Interpolate ( i )
import qualified Lib.Client.Translation.Translation
                                               as ClientTranslation

import qualified Lib.Client.Main               as Main
import qualified Lib.Client.DumpTab            as DumpTab
import           System.IO.Error                ( isUserError )
import qualified Lib.Client.DagsdatoTab        as DagsdatoTab
import qualified Lib.Client.DoneshootingTab        as DoneshootingTab
import qualified Lib.Client.DagsdatoBackupTab  as DagsdatoBackupTab
import qualified Lib.Client.PhotographersTab   as PhotographersTab
import qualified Lib.Client.CamerasTab         as CamerasTab
import qualified Lib.Client.ShootingsTab       as ShootingsTab
import qualified Lib.Client.SessionsTab       as SessionsTab
import qualified Lib.Client.Tabs       as ClientTabs

import qualified Foreign.JavaScript            as JavaScript
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , catchIOError
                                                )
import qualified Control.Monad.Except          as E
                                                ( catchError
                                                , throwError
                                                )
import qualified Relude.Unsafe                 as Unsafe
import qualified Data.HashMap.Strict           as HashMap
import qualified Lib.Message                   as Message
import           Utils.Comonad
import qualified Utils.ListZipper              as ListZipper
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                )
import qualified Control.Lens                  as Lens


import qualified Lib.Watchers                  as Watchers
import           Control.Concurrent             ( forkIO
                                                , killThread
                                                )
import           Lib.App                        ( runApp
                                                , AppEnv
                                                , readGrades
                                                , grab
                                                , readTranslation
                                                , readDumpDir
                                                , AppError(..)
                                                , WithError
                                                , IError(..)
                                                , Env(..)
                                                , Has(..)
                                                , HPhotographers(..)
                                                , MPhotographersFile(..)
                                                , HGrades(..)
                                                , MGradesFile(..)
                                                , HDumpDir(..)
                                                , HConfigDumpDir(..)
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
                                                , HBuild(..)
                                                , MBuildFile(..)
                                                , HSessions(..)
                                                , MSessionsFile(..)
                                                , HLocationFile(..)
                                                , MLocationFile(..)
                                                , HTranslationFile(..)
                                                , MTranslationFile(..)
                                                , HCameras(..)
                                                , MCamerasFile(..)
                                                , MStartMap(..)
                                                , WatchManager(..)
                                                , MStopMap(..)
                                                , OutChan(..)
                                                , InChan(..)
                                                , unMPhotographersFile
                                                , unHPhotographers
                                                , unMGradesFile
                                                , unHGrades
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

import qualified Lib.Server.Import           as ServerImport
import qualified Lib.Server.Download           as ServerDownload
import qualified Lib.Server.Build              as ServerBuild
import qualified Lib.Model.Build               as Build
import qualified Lib.Model.Grade               as Grade
import qualified Lib.Model.Doneshooting        as Doneshooting
import qualified Lib.Model.Camera              as Camera
import qualified Lib.Model.Dump                as Dump
import qualified Lib.Model.DumpDir             as DumpDir
import qualified Lib.Model.Tab                 as Tab
import qualified Lib.Model.Shooting            as Shooting
import qualified Lib.Model.Session             as Session
import qualified Lib.Model.Photographer        as Photographer
import qualified Lib.Model.Data                as Data
import qualified Lib.Model.Translation                as Translation
import qualified Lib.Model.Dagsdato            as Dagsdato
import qualified Lib.Model.DagsdatoBackup      as DagsdatoBackup
import qualified Lib.Model.Location            as Location

--------------------------------------------------------------------------------
data PhotographeesInputEntry = PhotographeesInputEntry
    { _elementPhotographeesInputTE :: !Element
    , _photographeesInputTE    :: !(Event Grade.Grades)
    }

instance Widget PhotographeesInputEntry where
    getElement = _elementPhotographeesInputTE


entryPhotographeeInput
    :: Behavior (Data.Data String Grade.Grades) -> UI PhotographeesInputEntry
entryPhotographeeInput bValue = do
    content      <- UI.div

    elemGrades   <- listBoxGrades bValue


    input        <- UI.input

    buttonInsert <- UI.button # set text "insert"

    bEditing     <- stepper False $ and <$> unions
        [True <$ UI.focus input, False <$ UI.blur input]

    item <- Reactive.currentValue bValue
    case item of
        Data.Data item' -> do
            void $ element input # set
                value
                (extract (Grade._unGrades item') ^. Grade.gradeId)
        _ -> return ()

    window <- askWindow

    liftIOLater $ Reactive.onChange bValue $ \s -> runUI window $ do
        case s of
            Data.NotAsked  -> void $ element content # set text "Not Asked"
            Data.Loading   -> void $ element content # set text "bobo"
            Data.Failure e -> do
                err <- string (show e)
                void $ element content # set children [] #+ [element err]
            Data.Data item' -> do
                editing <- liftIO $ currentValue bEditing
                when (not editing) $ void $ do
                    _ <- element input # set
                        value
                        (extract (Grade._unGrades item') ^. Grade.gradeId)
                    element content
                        #  set children []
                        #+ [ element input
                           , element buttonInsert
                           , element elemGrades
                           ]


    let eClick =
            filterJust
                $   mkNewGrade
                <$> (Data.toJust <$> bValue)
                <@  UI.click buttonInsert



    let eElemGrades = filterJust $ rumors $ _gradesPB elemGrades

    let _elementPhotographeesInputTE = content
    let photographeesInputTE1 =
            filterJust
                $   flap
                .   fmap setNameGrade
                <$> Data.toJust
                <$> bValue
                <@> UI.valueChange input
    let lol = unions [eClick, eElemGrades, photographeesInputTE1]
    let _photographeesInputTE = Unsafe.head <$> lol

    return PhotographeesInputEntry { .. }

insertPhotograhees
    :: Maybe Grade.Grades -> Grade.Photographees -> Maybe Grade.Grades
insertPhotograhees grades photographees = case grades of
    Nothing -> Nothing
    Just grades' ->
        let (ListZipper.ListZipper xs x ys) = Grade._unGrades grades'
        in  Just $ Grade.Grades $ ListZipper.ListZipper
                xs
                (Grade.Grade (x ^. Grade.gradeId) photographees)
                ys


-----------------------------------------------------------------------------
data PhotographeesBox a = PhotographeesBox
    { _elementPhotographeesPB   :: Element
    , _photographeesPB :: !(Tidings (Maybe Grade.Photographees))
    }

instance Widget (PhotographeesBox a) where
    getElement = _elementPhotographeesPB

listBoxPhotographees2
    :: Behavior (Data.Data String Grade.Photographees)
    -> UI (PhotographeesBox a)
listBoxPhotographees2 bitems = do
    _elementPhotographeesPB <- UI.div
    list                    <- UI.select

    item                    <- Reactive.currentValue bitems

    _ <- case item of
        Data.NotAsked  -> element _elementPhotographeesPB # set text "Not Asked"
        Data.Loading   -> element _elementPhotographeesPB # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            element _elementPhotographeesPB # set children [] #+ [element err]
        Data.Data item' -> do
            _ <- element list # set children [] #+ (mkPhotographees item')
            element _elementPhotographeesPB # set children [] #+ [element list]

    window   <- askWindow

    bEditing <- stepper False $ and <$> unions
        [True <$ UI.focus list, False <$ UI.blur list]

    liftIOLater $ Reactive.onChange bitems $ \s -> runUI window $ do
        case s of
            Data.NotAsked ->
                void $ element _elementPhotographeesPB # set text "Not Asked"
            Data.Loading ->
                void $ element _elementPhotographeesPB # set text "bobo"
            Data.Failure e -> do
                err <- string (show e)
                void
                    $  element _elementPhotographeesPB
                    #  set children []
                    #+ [element err]
            Data.Data item' -> void $ do
                editing <- liftIO $ currentValue bEditing
                when (not editing) $ void $ do
                    _ <- element list # set children [] #+ (mkPhotographees item')
                    element _elementPhotographeesPB
                        #  set children []
                        #+ [element list]

    let _photographeesPB =
            tidings (Data.toJust <$> bitems)
                $   selectPhotographeeF
                <$> (Data.toJust <$> bitems)
                <@> (filterJust (UI.selectionChange list))


    return PhotographeesBox { .. }


selectPhotographeeF
    :: Maybe Grade.Photographees -> Int -> Maybe Grade.Photographees
selectPhotographeeF photographees selected = case photographees of
    Nothing            -> Nothing
    Just photographees' -> asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex photographees'' -> if selected == thisIndex
            then Just (Grade.Photographees photographees'')
            else Nothing
        )
        (Grade._unPhotographees photographees')


mkPhotographees :: Grade.Photographees -> [UI Element]
mkPhotographees photographees' = do
    let zipper = Grade._unPhotographees photographees'
    let elems = ListZipper.iextend
            (\i photographees'' ->
                (i, zipper == photographees'', extract photographees'')
            )
            zipper
    fmap mkPhotographeesListItem (ListZipper.toList elems)


mkPhotographeesListItem :: (Int, Bool, Grade.Photographee) -> UI Element
mkPhotographeesListItem (thisIndex, isCenter, photographee) = do
    let name'  = photographee ^. Grade.name -- change 
    let option = UI.option # set value (show thisIndex) # set text name'
    if isCenter then option # set UI.selected True else option



--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
data PhotographeesSide = PhotographeesSide
    { _elementPhotographeesSideTE :: !Element
    , _photographeesSideTE    :: !(Event Grade.Photographees)
    }

instance Widget PhotographeesSide where
    getElement = _elementPhotographeesSideTE


listBoxPhotographees
    :: Behavior (Data.Data String Grade.Photographees) -> UI PhotographeesSide
listBoxPhotographees bValue = do
    content      <- UI.div

    name         <- UI.input
    tid          <- UI.input
    sys          <- UI.input

    select       <- listBoxPhotographees2 bValue

    buttonInsert <- UI.button # set text "insert"

    bEditingName <- stepper False $ and <$> unions
        [True <$ UI.focus name, False <$ UI.blur name]

    bEditingTid <- stepper False $ and <$> unions
        [True <$ UI.focus tid, False <$ UI.blur tid]

    bEditingSys <- stepper False $ and <$> unions
        [True <$ UI.focus sys, False <$ UI.blur sys]

    item <- Reactive.currentValue bValue
    case item of
        Data.Data item' -> do
            void $ do
                _ <- element name # set
                    value
                    (extract (Grade._unPhotographees item') ^. Grade.name)

                _ <- element tid # set
                    value
                    (extract (Grade._unPhotographees item') ^. Grade.tid)

                element sys # set
                    value
                    (extract (Grade._unPhotographees item') ^. Grade.sys)
        _ -> return ()

    window <- askWindow

    liftIOLater $ Reactive.onChange bValue $ \s -> runUI window $ do
        case s of
            Data.NotAsked  -> void $ element content # set text "Not Asked"
            Data.Loading   -> void $ element content # set text "bobo"
            Data.Failure e -> do
                err <- string (show e)
                void $ element content # set children [] #+ [element err]
            Data.Data item' -> do
                editingName <- liftIO $ currentValue bEditingName
                editingSys  <- liftIO $ currentValue bEditingSys
                editingTid  <- liftIO $ currentValue bEditingTid

                when (not editingName) $ void $ do
                    element name # set
                        value
                        (extract (Grade._unPhotographees item') ^. Grade.name)

                when (not editingSys) $ void $ do
                    element sys # set
                        value
                        (extract (Grade._unPhotographees item') ^. Grade.sys)

                when (not editingTid) $ void $ do
                    element tid # set
                        value
                        (extract (Grade._unPhotographees item') ^. Grade.tid)

                when (not (editingName || editingTid || editingSys)) $ void $ do

                    element content
                        #  set children []
                        #+ [ element name
                           , element sys
                           , element tid
                           , element buttonInsert
                           , element select
                           ]

    let eClick =
            filterJust
                $   mkNewPhotograhee
                <$> (Data.toJust <$> bValue)
                <@  UI.click buttonInsert

    let _elementPhotographeesSideTE = content
    let gradeInputTE1Name =
            filterJust
                $   flap
                .   fmap setNamePhotographee
                <$> Data.toJust
                <$> bValue
                <@> UI.valueChange name

    let gradeInputTE1Tid =
            filterJust
                $   flap
                .   fmap setTidPhotographee
                <$> Data.toJust
                <$> bValue
                <@> UI.valueChange tid

    let gradeInputTE1Sys =
            filterJust
                $   flap
                .   fmap setSysPhotographee
                <$> Data.toJust
                <$> bValue
                <@> UI.valueChange sys


    let lol = unions
            [ eClick
            , gradeInputTE1Name
            , gradeInputTE1Tid
            , gradeInputTE1Sys
            , filterJust $ rumors $ _photographeesPB select
            ]
    let _photographeesSideTE = Unsafe.head <$> lol

    return PhotographeesSide { .. }

setNamePhotographee :: Grade.Photographees -> String -> Grade.Photographees
setNamePhotographee photographees name =
    case Grade._unPhotographees photographees of
        ListZipper.ListZipper ls x rs ->
            Grade.Photographees
                $ ListZipper.ListZipper ls (x & Grade.name .~ name) rs

setTidPhotographee :: Grade.Photographees -> String -> Grade.Photographees
setTidPhotographee photographees tid =
    case Grade._unPhotographees photographees of
        ListZipper.ListZipper ls x rs ->
            Grade.Photographees
                $ ListZipper.ListZipper ls (x & Grade.tid .~ tid) rs

setSysPhotographee :: Grade.Photographees -> String -> Grade.Photographees
setSysPhotographee photographees sys =
    case Grade._unPhotographees photographees of
        ListZipper.ListZipper ls x rs ->
            Grade.Photographees
                $ ListZipper.ListZipper ls (x & Grade.sys .~ sys) rs

mkNewPhotograhee :: Maybe Grade.Photographees -> Maybe Grade.Photographees
mkNewPhotograhee photographees' = case photographees' of
    Nothing -> Nothing
    Just zs ->
        let (ListZipper.ListZipper xs x ys) = Grade._unPhotographees zs
        in  Just $ Grade.Photographees $ ListZipper.ListZipper
                xs
                (Grade.Photographee "" "" "")
                (x : ys)


--------------------------------------------------------------------------------
data GradeInputEntry = GradeInputEntry
    { _elementGradeInputTE :: !Element
    , _gradeInputTE    :: !(Event Grade.Grades)
    }

instance Widget GradeInputEntry where
    getElement = _elementGradeInputTE


entryGradeInput
    :: Behavior (Data.Data String Grade.Grades) -> UI GradeInputEntry
entryGradeInput bValue = do
    content      <- UI.div
    input        <- UI.input

    buttonInsert <- UI.button # set text "insert"

    bEditing     <- stepper False $ and <$> unions
        [True <$ UI.focus input, False <$ UI.blur input]

    item <- Reactive.currentValue bValue
    case item of
        Data.Data item' -> do
            void $ element input # set
                value
                (extract (Grade._unGrades item') ^. Grade.gradeId)
        _ -> return ()

    window <- askWindow

    liftIOLater $ Reactive.onChange bValue $ \s -> runUI window $ do
        case s of
            Data.NotAsked  -> void $ element content # set text "Not Asked"
            Data.Loading   -> void $ element content # set text "bobo"
            Data.Failure e -> do
                err <- string (show e)
                void $ element content # set children [] #+ [element err]
            Data.Data item' -> do
                editing <- liftIO $ currentValue bEditing
                when (not editing) $ void $ do
                    _ <- element input # set
                        value
                        (extract (Grade._unGrades item') ^. Grade.gradeId)
                    element content
                        #  set children []
                        #+ [element input, element buttonInsert]


    let eClick =
            filterJust
                $   mkNewGrade
                <$> (Data.toJust <$> bValue)
                <@  UI.click buttonInsert

    let _elementGradeInputTE = content
    let gradeInputTE1 =
            filterJust
                $   flap
                .   fmap setNameGrade
                <$> Data.toJust
                <$> bValue
                <@> UI.valueChange input
    let lol           = unions [eClick, gradeInputTE1]
    let _gradeInputTE = Unsafe.head <$> lol

    return GradeInputEntry { .. }

setNameGrade :: Grade.Grades -> String -> Grade.Grades
setNameGrade grades gradeId = case Grade._unGrades grades of
    ListZipper.ListZipper ls x rs -> Grade.Grades
        $ ListZipper.ListZipper ls (x & Grade.gradeId .~ gradeId) rs

mkNewGrade :: Maybe Grade.Grades -> Maybe Grade.Grades
mkNewGrade grades' = case grades' of
    Nothing -> Nothing
    Just zs ->
        let (ListZipper.ListZipper xs x ys) = Grade._unGrades zs
        in  Just $ Grade.Grades $ ListZipper.ListZipper
                xs
                (Grade.Grade
                    ""
                    (Grade.Photographees $ ListZipper.ListZipper
                        []
                        (Grade.Photographee "" "" "")
                        []
                    )
                )
                (x : ys)

-----------------------------------------------------------------------------
data GradesBox a = GradesBox
    { _elementGradesPB   :: Element
    , _gradesPB :: !(Tidings (Maybe Grade.Grades))
    }

instance Widget (GradesBox a) where
    getElement = _elementGradesPB

listBoxGrades :: Behavior (Data.Data String Grade.Grades) -> UI (GradesBox a)
listBoxGrades bitems = do
    _elementGradesPB <- UI.div
    list             <- UI.select

    item             <- Reactive.currentValue bitems

    _ <- case item of
        Data.NotAsked  -> element _elementGradesPB # set text "Not Asked"
        Data.Loading   -> element _elementGradesPB # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            element _elementGradesPB # set children [] #+ [element err]
        Data.Data item' -> do
            _ <- element list # set children [] #+ (mkGrades item')
            element _elementGradesPB # set children [] #+ [element list]

    window   <- askWindow

    bEditing <- stepper False $ and <$> unions
        [True <$ UI.focus list, False <$ UI.blur list]

    liftIOLater $ Reactive.onChange bitems $ \s -> runUI window $ do
        case s of
            Data.NotAsked ->
                void $ element _elementGradesPB # set text "Not Asked"
            Data.Loading   -> void $ element _elementGradesPB # set text "bobo"
            Data.Failure e -> do
                err <- string (show e)
                void
                    $  element _elementGradesPB
                    #  set children []
                    #+ [element err]
            Data.Data item' -> void $ do
                editing <- liftIO $ currentValue bEditing
                when (not editing) $ void $ do
                    _ <- element list # set children [] #+ (mkGrades item')
                    element _elementGradesPB # set children [] #+ [element list]

    let _gradesPB =
            tidings (Data.toJust <$> bitems)
                $   selectGradeF
                <$> (Data.toJust <$> bitems)
                <@> (filterJust (UI.selectionChange list))


    return GradesBox { .. }


selectGradeF :: Maybe Grade.Grades -> Int -> Maybe Grade.Grades
selectGradeF grades selected = case grades of
    Nothing     -> Nothing
    Just grades' -> asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex grades'' -> if selected == thisIndex
            then Just (Grade.Grades grades'')
            else Nothing
        )
        (Grade._unGrades grades')


mkGrades :: Grade.Grades -> [UI Element]
mkGrades grades' = do
    let zipper = Grade._unGrades grades'
    let elems = ListZipper.iextend
            (\i grades'' -> (i, zipper == grades'', extract grades''))
            zipper
    fmap mkGradeListItem (ListZipper.toList elems)


mkGradeListItem :: (Int, Bool, Grade.Grade) -> UI Element
mkGradeListItem (thisIndex, isCenter, grade) = do
    let name'  = grade ^. Grade.gradeId -- change 
    let option = UI.option # set value (show thisIndex) # set text name'
    if isCenter then option # set UI.selected True else option




--------------------------------------------------------------------------------
data LocationBox a = LocationBox
    { _elementLocationPB   :: Element
    , _locationPB :: !(Tidings (Maybe Location.Location))
    }

instance Widget (LocationBox a) where
    getElement = _elementLocationPB



listBoxLocation
    :: Behavior (Data.Data String Location.Location) -> UI (LocationBox a)
listBoxLocation bitems = do
    _elementLocationPB <- UI.div

    selector           <- UI.button

    _ <- element _elementLocationPB # sink (locationItem selector) bitems


    let _locationPB = -- NOT SURE ABOUT ANY OF THIS SHITE.
            tidings (Data.toJust <$> bitems)
                $  (Data.toJust <$> bitems)
                <@ UI.click selector

    return LocationBox { .. }


locationItem :: (Show a, Widget w) => w -> WriteAttr Element (Data.Data a Location.Location)
locationItem filePicker = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            _ <- element filePicker # set children [] #+ [mkFilePicker5 item]
            return x # set children [] #+ [element filePicker]

mkFilePicker5 :: Location.Location -> UI Element
mkFilePicker5 location = do
    let name' = Lens.view Location.unLocation location
    UI.p # set text name'


--------------------------------------------------------------------------------


data TabsBox a = TabsBox
    { _tabsE :: Element
    , _tabsPB :: Event Tab.Tabs
    , _eDump :: Event ()
    , _eDagsdato :: Event ()
    , _eDoneshooting :: Event ()
    , _eDagsdatoBackup :: Event ()
    , _ePhotographers :: Event Photographer.Photographers
    , _eCameras :: Event Camera.Cameras
    , _eShootings :: Event Shooting.Shootings
    , _eSessions :: Event Session.Sessions
    , _eDownload :: Event ()
    , _eImporter :: Event ()
    , _eTranlation :: Event Translation.Translations
    }

instance Widget (TabsBox a) where
    getElement = _tabsE





format' :: String -> [String] -> String
format' code args = go code
    where
    at xs i = maybe " " id $ atMay xs i
    argument i = args `at` i

    go []           = []
    go ('%':'%':cs) = '%' : go cs
    go ('%':c  :cs) = argument index ++ go cs
        where index = fromEnum c - fromEnum '1'
    go (c:cs)       = c : go cs


format :: PrintfType a => String -> a
format x = fancy (format' x)

class PrintfType a where
    fancy :: ([String] -> String) -> a

instance PrintfType String where
    fancy f = f []

instance (PrintfType r) => PrintfType (String -> r) where
    fancy f x = fancy $ \xs -> f (x:xs)




tabsBox
    :: Behavior Translation.Translations
    -> Behavior (Data.Data String Tab.Tabs)
    -> Behavior (Data.Data String Photographer.Photographers)
    -> Behavior (Data.Data String Shooting.Shootings)
    -> Behavior (Data.Data String Dump.Dump)
    -> Behavior (Data.Data String Dagsdato.Dagsdato)
    -> Behavior (Data.Data String Camera.Cameras)
    -> Behavior (Data.Data String Doneshooting.Doneshooting)
    -> Behavior
           (Data.Data String DagsdatoBackup.DagsdatoBackup)
    -> Behavior (Data.Data String Session.Sessions)
    -> Behavior (Data.Data String Location.Location)
    -> Behavior (Data.Data String Grade.Grades)
    -> Behavior
           (Data.Data String DumpDir.DumpDir)
    -> Behavior
           (Data.Data String Build.Build)
    -> UI
           ( LocationBox h
           , GradesBox i
           , GradeInputEntry
           , PhotographeesInputEntry
           , PhotographeesSide
           , Main.MainTab
           , TabsBox a
           )
tabsBox bTranslations bTabs bPhotographers bShootings bDump bDagsdato bCameras bDoneshooting bDagsdatoBackup bSessions bLocation bGrades bDumpDir bBuild
    = mdo
        _tabsE     <- UI.div
        _ss        <- UI.div


        let dave = "Dave" :: String
        (errorView, _eTransError) <- ClientTranslation.translation2 bTranslations bMode (pure "error") (pure (\v ->  format v dave)) --(\v -> format v "dave"))
        (loadingView, _eTransLoading) <- ClientTranslation.translation2 bTranslations bMode (pure "loading") (pure id)
        (notAskedView, _eTransNotAsked) <- ClientTranslation.translation2 bTranslations bMode (pure "notAsked") (pure id)

        photographers <- PhotographersTab.photographersTab errorView loadingView notAskedView bPhotographers


        -----------------------------------------------------------------------
        camerasTrans <- mapM (\c -> do
                                translation <- ClientTranslation.translation2 bTranslations bMode (pure (show c)) (pure id)
                                return (c, translation)
                             ) $ [ Camera.CR2 , Camera.CR3 ]

        let camerasTranslations' = camerasTrans <&> (\(k,(v,_)) -> (\v' -> (k,v')) <$> v)
        -- (key, UI children)
        let camerasTranslations'' = sequenceA camerasTranslations'

        let bZipperCam = Lens.view Camera.unCameras <<$>> bCameras

        let lookups' _key []          =  Nothing
            lookups' key ((x,y):xys)
                            | key == x           =  Just y
                            | otherwise         =  lookups' key xys

        let ggCam = (\zip' trans ->  fmap (\z -> fmap (\t -> (t, Unsafe.fromJust (lookups' t trans ))) z ) zip') <$> bZipperCam <*> camerasTranslations''

        cameras       <- CamerasTab.camerasTab (facts tTranslations) (facts tMode)errorView loadingView notAskedView ggCam
        -----------------------------------------------------------------------
        shootingsTrans <- mapM (\c -> do
                                translation <- ClientTranslation.translation2 bTranslations bMode (pure (show c)) (pure id)
                                return (c, translation)
                             ) $ [ Shooting.Normal , Shooting.ReShoot ]

        let shootingsTranslations' = shootingsTrans <&> (\(k,(v,_)) -> (\v' -> (k,v')) <$> v)
        -- (key, UI children)
        let shootingsTranslations'' = sequenceA shootingsTranslations'

        let bZipperShoot = Lens.view Shooting.unShootings <<$>> bShootings


        let ggShoot = (\zip' trans ->  fmap (\z -> fmap (\t -> (t, Unsafe.fromJust (lookups' t trans ))) z ) zip') <$> bZipperShoot <*> shootingsTranslations''

        shootings       <- ShootingsTab.shootingsTab (facts tTranslations) (facts tMode)errorView loadingView notAskedView ggShoot
        --
        -----------------------------------------------------------------------
        sessionsTrans <- mapM (\c -> do
                                translation <- ClientTranslation.translation2 bTranslations bMode (pure (show c)) (pure id)
                                return (c, translation)
                             ) $ [ Session.KindergartenGroup, Session.KindergartenSingle,Session.School]

        let sessionsTranslations' = sessionsTrans <&> (\(k,(v,_)) -> (\v' -> (k,v')) <$> v)
        -- (key, UI children)
        let sessionsTranslations'' = sequenceA sessionsTranslations'

        let bZipperSessions = Lens.view Session.unSessions <<$>> bSessions

        let ggSession = (\zip' trans ->  fmap (\z -> fmap (\t -> (t, Unsafe.fromJust (lookups' t trans ))) z ) zip') <$> bZipperSessions <*> sessionsTranslations''

        sessions       <- SessionsTab.sessionsTab (facts tTranslations) (facts tMode)errorView loadingView notAskedView ggSession


        -----------------------------------------------------------------------
        (pickView, _ePick) <- ClientTranslation.translation3  bTranslations bMode (pure "pick") (pure id)
        elemDump       <- DumpTab.dumpTab (facts tTranslations) (facts tMode)errorView loadingView notAskedView pickView bDump
        -----------------------------------------------------------------------
        elemDagsdato       <- DagsdatoTab.dagsdatoTab (facts tTranslations) (facts tMode)errorView loadingView notAskedView pickView bDagsdato

        -----------------------------------------------------------------------
        elemDagsdatoBackup <- DagsdatoBackupTab.dagsdatoBackupTab (facts tTranslations) (facts tMode)errorView loadingView notAskedView pickView bDagsdatoBackup
        -----------------------------------------------------------------------
        elemDoneshooting <- DoneshootingTab.doneshootingTab (facts tTranslations) (facts tMode)errorView loadingView notAskedView pickView bDoneshooting


        -----------------------------------------------------------------------
        let tabsy = [ Tab.DumpTab
                , Tab.DagsdatoTab
                , Tab.DagsdatoBackupTab
                , Tab.DoneshootingTab
                , Tab.DoneshootingBackupTab
                , Tab.PhotographersTab
                , Tab.CamerasTab
                , Tab.ShootingsTab
                , Tab.SessionsTab
                , Tab.LocationTab
                , Tab.MainTab
                , Tab.InsertPhotographeeTab
                , Tab.ControlTab
                ]

    --
    -- x == (key, bview, (evalue, bkey))
        transTABS <- mapM (\t -> do
            translation <- ClientTranslation.translation2 bTranslations bMode (pure (show t)) (pure id)
            return (t, translation)
            ) tabsy

        let kv = transTABS <&> (\(_,(_,x)) -> x) <&> (\item -> (\m k v -> HashMap.insert k v m) <$> (Lens.view Translation.unTranslation <$> bTranslations) <*> (snd item) <@> (fst item))
        let kv2 = camerasTrans <&> (\(_,(_,x)) -> x) <&> (\item -> (\m k v -> HashMap.insert k v m) <$> (Lens.view Translation.unTranslation <$> bTranslations) <*> (snd item) <@> (fst item))
        let kv3 = shootingsTrans <&> (\(_,(_,x)) -> x) <&> (\item -> (\m k v -> HashMap.insert k v m) <$> (Lens.view Translation.unTranslation <$> bTranslations) <*> (snd item) <@> (fst item))
        let kv4 = sessionsTrans <&> (\(_,(_,x)) -> x) <&> (\item -> (\m k v -> HashMap.insert k v m) <$> (Lens.view Translation.unTranslation <$> bTranslations) <*> (snd item) <@> (fst item))
        -----------------------------------------------------------------------


        importer <- UI.button #. "button" # set text "importer"
        download <- UI.button #. "button" # set text "download"
        switchMode <- UI.button #. "button" # set text "skift"
        extraMenu <- UI.div #. "buttons has-addons" # set children [importer, download, switchMode]

        let eSwitchMode = UI.click switchMode

        bMode <-
            stepper ClientTranslation.Normal
            $   ClientTranslation.toggle
            <$> bMode
            <@  eSwitchMode

        let tMode = tidings bMode (bMode <@ eSwitchMode)


        let eTranslation = Unsafe.head <$> unions ([ (\m k v -> HashMap.insert k v m) <$> (Lens.view Translation.unTranslation <$> bTranslations) <*> (snd $ _eTransError ) <@> (fst $ _eTransError )
                                         , (\m k v -> HashMap.insert k v m) <$> (Lens.view Translation.unTranslation <$> bTranslations) <*> (snd $ _eTransLoading ) <@> (fst $ _eTransLoading )
                                         , (\m k v -> HashMap.insert k v m) <$> (Lens.view Translation.unTranslation <$> bTranslations) <*> (snd $ _eTransNotAsked ) <@> (fst $ _eTransNotAsked )
                                         , (\m k v -> HashMap.insert k v m) <$> (Lens.view Translation.unTranslation <$> bTranslations) <*> (snd $ _ePick ) <@> (fst $ _ePick )
                                         ]<>kv<>kv2<>kv3<>kv4)

        let tTranslations = tidings bTranslations (Translation.Translations <$> eTranslation)


        tabs <- ClientTabs.tabs (facts tTranslations) (facts tMode) transTABS bTabs 
        let eSelection = ClientTabs._selection tabs





















        elemLocation           <- listBoxLocation bLocation
        elemGrades             <- listBoxGrades bGrades
        elemGradesInput        <- entryGradeInput bGrades
        elemPhotograheesInput  <- entryPhotographeeInput bGrades
        elemPhotograheesInput2 <- listBoxPhotographees
            (   fmap (\x -> extract (Grade._unGrades x) ^. Grade.photographees)
            <$> bGrades
            )

        elemMainTab <- Main.mainTab bGrades bDumpDir bBuild




        _ <- element _ss
            # sink
                  (tabItems photographers
                            shootings
                            elemDump
                            elemDagsdato
                            cameras
                            elemDoneshooting
                            elemDagsdatoBackup
                            sessions
                            elemLocation
                            elemGrades
                            elemGradesInput
                            elemPhotograheesInput
                            elemPhotograheesInput2
                            elemMainTab
                  ) bTabs



        menu <- element tabs

        window <- askWindow

        padder1 <- UI.br
        padder2  <- UI.br
        liftIOLater $ runUI window $ do
            s <- currentValue bPhotographers
            element _tabsE # set children [menu, padder1, _ss]
            forM_ s $ \s' -> do
                let photographers' = Lens.view Photographer.unPhotographers s'
                if (ListZipper.isLeft photographers') then
                    element _tabsE # set children [menu, padder1, _ss, padder2, extraMenu]
                else
                    return _tabsE

        liftIOLater $ Reactive.onChange bPhotographers $ \s -> runUI window $ do
            element _tabsE # set children [menu, padder1, _ss]
            forM_ s $ \s' -> do
                let photographers' = Lens.view Photographer.unPhotographers s'
                if (ListZipper.isLeft photographers') then
                    element _tabsE # set children [menu, padder1, _ss, padder2, extraMenu]
                else
                    return _tabsE


        let _tabsPB = eSelection
        let _eDump           = DumpTab._selection elemDump
        let _eDagsdato       = DagsdatoTab._selection elemDagsdato
        let _eDagsdatoBackup = DagsdatoBackupTab._selection elemDagsdatoBackup
        let _ePhotographers  = PhotographersTab._selection photographers
        let _eCameras        = CamerasTab._selection cameras
        let _eShootings = ShootingsTab._selection shootings
        let _eSessions = SessionsTab._selection sessions
        let _eDoneshooting = DoneshootingTab._selection elemDoneshooting

        let _eDownload = UI.click download
        let _eImporter = UI.click importer
        let _eTranlation = Translation.Translations <$> eTranslation
        
        return
            ( elemLocation
            , elemGrades
            , elemGradesInput
            , elemPhotograheesInput
            , elemPhotograheesInput2
            , elemMainTab
            , TabsBox { .. }
            )



tabItems :: (Show a, Widget w1, Widget w2, Widget w3, Widget w4,
                   Widget w5, Widget w6, Widget w7, Widget w8, Widget w9, Widget w10,
                   Widget w11, Widget w12, Widget w13, Widget w14) =>
                  w12
                  -> w2
                  -> w8
                  -> w10
                  -> w7
                  -> w9
                  -> w11
                  -> w6
                  -> w3
                  -> w4
                  -> w5
                  -> w13
                  -> w14
                  -> w1
                  -> WriteAttr Element (Data.Data a Tab.Tabs)
tabItems photographers shootings dump dagsdato cameras doneshooting dagsdatoBackup sessions location grades gradesInput photographeesInput photographeesInput2 mainTab
    = mkWriteAttr $ \i x -> void $ do
        case i of
            Data.NotAsked  -> return x # set text "Not Asked"
            Data.Loading   -> return x # set text "bobo"
            Data.Failure e -> do
                err <- string (show e)
                return x # set children [] #+ [element err]
            Data.Data item -> do
                case extract (Lens.view Tab.unTabs item) of
                    Tab.MainTab -> do
                        return x
                            #  set children [] -- THIS IS DANGEROUS?
                            #+ [element mainTab]

                    Tab.ShootingsTab -> do
                        return x # set children [] #+ [element shootings]

                    Tab.LocationTab -> do
                        return x
                            #  set children []
                            #+ [ element location
                               , element grades
                               , element gradesInput
                               ]

                    Tab.SessionsTab -> do
                        return x # set children [] #+ [element sessions]

                    Tab.CamerasTab -> do
                        return x # set children [] #+ [element cameras]

                    Tab.DumpTab -> do
                        return x # set children [] #+ [element dump]

                    Tab.DoneshootingTab -> do
                        return x # set children [] #+ [element doneshooting]

                    Tab.DagsdatoTab -> do
                        return x # set children [] #+ [element dagsdato]

                    Tab.DagsdatoBackupTab -> do
                        return x # set children [] #+ [element dagsdatoBackup]

                    Tab.PhotographersTab -> do
                        return x # set children [] #+ [element photographers]

                    Tab.InsertPhotographeeTab -> do
                        return x
                            #  set children []
                            #+ [ element photographeesInput
                               , element photographeesInput2
                               ]


                    _ -> do
                        return x # set children [] #+ []

--------------------------------------------------------------------------------
example :: [String] -> JavaScript.JSObject -> JSFunction ()
example options callback = ffi
    "require('electron').remote.dialog.showOpenDialog({properties: %2}).then(result => %1(result.filePaths[0]))"
    callback
    options


example2 :: [String] -> JavaScript.JSObject -> JSFunction ()
example2 options callback = ffi
    "require('electron').remote.dialog.showSaveDialog().then(result => %1(result.filePath))"
    callback
    options




setup :: AppEnv -> Window -> UI ()
setup env@Env {..} win = mdo

    {-
    _ <- liftIO $ (writeFile "docs.md" . markdown) streamDocs

    baseUrl <- liftIO $ parseBaseUrl "http://localhost"
    manager <- liftIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager (baseUrl { baseUrlPort = 9000 })

    _ <- liftIO $ printSourceIO clientEnv photographersStream
    -}




    _ <- return win # set title "FF"


    (elemLocation, elemGrades, elemGradesInput, elemPhotograheesInput, elemPhotograheesInput2, mainTab, elem3) <-
        tabsBox bTranslations bTabs
                bPhotographers
                bShootings
                bDumpDir
                bDagsdatoDir
                bCameras
                bDoneshootingDir
                bDagsdatoBackupDir
                bSessions
                bLocationFile
                bGrades
                bConfigDumpDir
                bBuild

    let eElemPhotographees2 =
            filterJust
                $   insertPhotograhees
                <$> (Data.toJust <$> bGrades)
                <@> _photographeesSideTE elemPhotograheesInput2

    let fxRunBuild =
            (\file -> when (file /= "") $ liftIO $ void $ Chan.writeChan
                (unInChan inChan)
                (Message.RunDownload file)
            ) :: FilePath -> IO ()

    callbackRunBuild <- ffiExport fxRunBuild

    _ <- onEvent (_eDownload elem3) $ \_ -> do
        runFunction $ example2 [] callbackRunBuild


    let fxRunImporter =
            (\folder -> when (folder /= "") $ liftIO $ void $ Chan.writeChan
                (unInChan inChan)
                (Message.RunImporter folder)
            ) :: FilePath -> IO ()

    callbackRunImporter <- ffiExport fxRunImporter

    _ <- onEvent (_eImporter elem3) $ \_ -> do
        runFunction $ example ["openFile"] callbackRunImporter



    _ <- onEvent eElemPhotographees2 $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteGrades item)


    let eMainTab = Main._eMainTab mainTab
    _ <- onEvent eMainTab $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteGrades item)


    let eTranslation = _eTranlation elem3
    _ <- onEvent eTranslation $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteTranslation item)


    let eBuild' = Main._eBuild mainTab
    _ <- onEvent eBuild' $ \_ -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan) (Message.RunBuild)


    let ePhotographers' = _ePhotographers elem3
    _ <- onEvent ePhotographers' $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WritePhographers item)

    let eShootings' = _eShootings elem3
    _ <- onEvent eShootings' $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteShootings item)

    let eSessions' = _eSessions elem3
    _ <- onEvent eSessions' $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteSessions item)

    let eElemGrades = filterJust $ rumors $ _gradesPB elemGrades
    _ <- onEvent eElemGrades $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteGrades item)

    let eElemGradesInput = _gradeInputTE elemGradesInput
    _ <- onEvent eElemGradesInput $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteGrades item)

    let eElemPhotographeesInput = _photographeesInputTE elemPhotograheesInput
    _ <- onEvent eElemPhotographeesInput $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteGrades item)

    let fx =
            (\folder -> when (folder /= "") $ liftIO $ void $ Chan.writeChan
                (unInChan inChan)
                (Message.WriteDump (Dump.Dump folder))
            ) :: FilePath -> IO ()

    callback <- ffiExport fx
    let eDump = _eDump elem3
    _ <- onEvent eDump $ \_ -> do
        runFunction $ example ["openDirectory"] callback

    ------------------------------------------------------------------------------
    let fx2 =
            (\folder -> when (folder /= "") $ liftIO $ void $ Chan.writeChan
                (unInChan inChan)
                (Message.WriteDagsdato (Dagsdato.Dagsdato folder))
            ) :: FilePath -> IO ()

    callback2 <- ffiExport fx2
    let eDagsdato = _eDagsdato elem3
    _ <- onEvent eDagsdato $ \_ -> do
        runFunction $ example ["openDirectory"] callback2

    ------------------------------------------------------------------------------
    let fx3 =
            (\folder -> when (folder /= "") $ liftIO $ void $ Chan.writeChan
                (unInChan inChan)
                (Message.WriteDoneshooting (Doneshooting.Doneshooting folder))
            ) :: FilePath -> IO ()

    callback3 <- ffiExport fx3
    
    let eDoneshooting = _eDoneshooting elem3
    _ <- onEvent eDoneshooting $ \_ -> do
        runFunction $ example ["openDirectory"] callback3

    ------------------------------------------------------------------------------
    let
        fx4 =
            (\folder -> when (folder /= "") $ liftIO $ void $ Chan.writeChan
                (unInChan inChan)
                (Message.WriteDagsdatoBackup
                    (DagsdatoBackup.DagsdatoBackup folder)
                )
            ) :: FilePath -> IO ()

    callback4 <- ffiExport fx4
    let eDagsdatoBackup = _eDagsdatoBackup elem3
    _ <- onEvent eDagsdatoBackup $ \_ -> do
        runFunction $ example ["openDirectory"] callback4

    ------------------------------------------------------------------------------
    let fx5 =
            (\f -> when (f /= "") $ liftIO $ void $ Chan.writeChan
                (unInChan inChan)
                (Message.WriteLocation (Location.Location f))
            ) :: FilePath -> IO ()

    callback5 <- ffiExport fx5
    let eElemLocation = filterJust $ rumors $ _locationPB elemLocation
    _ <- onEvent eElemLocation $ \_-> do
        runFunction $ example ["openFile"] callback5

    ------------------------------------------------------------------------------

    let eElem3 = _tabsPB elem3
    _ <- onEvent eElem3 $ \item -> do
        liftIO $ void $ Chan.writeChan (unInChan inChan)
                                       (Message.WriteTabs item)

    let eCameras' = _eCameras elem3
    _ <- onEvent eCameras' $ \item -> do
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
      , Has HGrades r
      , Has MGradesFile r
      , Has HSessions r
      , Has MSessionsFile r
      , Has HCameras r
      , Has MCamerasFile r
      , Has HDumpDir r
      , Has HBuild r
      , Has HConfigDumpDir r
      , Has MDumpFile r
      , Has HLocationFile r
      , Has MLocationFile r
      , Has HTranslationFile r
      , Has MTranslationFile r
      , Has HDagsdatoDir r
      , Has MDagsdatoFile r
      , Has HDagsdatoBackupDir r
      , Has MDagsdatoBackupFile r
      , Has HDoneshootingDir r
      , Has MDoneshootingFile r
      , Has MBuildFile r
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

    let key           = "photographers"
    let watcher       = Watchers.photographersFile
    let newStartMap   = HashMap.insert key watcher startMap

    let key2          = "tabs"
    let watcher2      = Watchers.tabsFile
    let newStartMap2 = HashMap.insert key2 watcher2 newStartMap

    let key3          = "shootings"
    let watcher3      = Watchers.shootingsFile
    let newStartMap3 = HashMap.insert key3 watcher3 newStartMap2

    let key4          = "dump"
    let watcher4      = Watchers.dumpFile
    let newStartMap4 = HashMap.insert key4 watcher4 newStartMap3

    let key5          = "dagsdato"
    let watcher5      = Watchers.dagsdatoFile
    let newStartMap5 = HashMap.insert key5 watcher5 newStartMap4

    let key6          = "cameras"
    let watcher6      = Watchers.camerasFile
    let newStartMap6 = HashMap.insert key6 watcher6 newStartMap5

    let key7          = "doneshooting"
    let watcher7      = Watchers.doneshootingFile
    let newStartMap7 = HashMap.insert key7 watcher7 newStartMap6

    let key8          = "dagsdatoBackup"
    let watcher8      = Watchers.dagsdatoBackupFile
    let newStartMap8 = HashMap.insert key8 watcher8 newStartMap7

    let key9          = "sessions"
    let watcher9      = Watchers.sessionsFile
    let newStartMap9 = HashMap.insert key9 watcher9 newStartMap8

    let key10         = "location"
    let watcher10     = Watchers.locationFile
    let newStartMap10 = HashMap.insert key10 watcher10 newStartMap9

    let key11         = "grades"
    let watcher11     = Watchers.gradesFile
    let newStartMap11 = HashMap.insert key11 watcher11 newStartMap10

    let key12         = "dumpDir"
    let watcher12     = Watchers.dumpDir `catchIOError` (\e -> do
                           traceShowM e
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       ) `E.catchError` (\e -> do
                                    traceShowM e
                                    return $ return () )

    let newStartMap12 = HashMap.insert key12 watcher12 newStartMap11

    let key13         = "build"
    let watcher13     = Watchers.buildFile
    let newStartMap13 = HashMap.insert key13 watcher13 newStartMap12

    let key14         = "translation"
    let watcher14     = Watchers.translationFile
    let newStartMap14 = HashMap.insert key14 watcher14 newStartMap13

    putMVar mStartMap newStartMap14


read :: forall  r m . WithChan r m => m ()
read = do
    inChan <- unInChan <$> grab @InChan
    liftIO $ Chan.writeChan inChan Message.ReadPhotographers
    liftIO $ Chan.writeChan inChan Message.ReadTabs
    liftIO $ Chan.writeChan inChan Message.ReadShootings
    liftIO $ Chan.writeChan inChan Message.ReadSessions
    liftIO $ Chan.writeChan inChan Message.ReadDump
    liftIO $ Chan.writeChan inChan Message.ReadDumpDir
    liftIO $ Chan.writeChan inChan Message.ReadDagsdato
    liftIO $ Chan.writeChan inChan Message.ReadCameras
    liftIO $ Chan.writeChan inChan Message.ReadDoneshooting
    liftIO $ Chan.writeChan inChan Message.ReadDagsdatoBackup
    liftIO $ Chan.writeChan inChan Message.ReadLocation
    liftIO $ Chan.writeChan inChan Message.ReadGrades
    liftIO $ Chan.writeChan inChan Message.ReadBuild
    liftIO $ Chan.writeChan inChan Message.ReadTranslation

startStartMap :: forall  r m . WithChan r m => m ()
startStartMap = do
    inChan <- unInChan <$> grab @InChan
    liftIO $ Chan.writeChan inChan Message.StartPhotograpers
    liftIO $ Chan.writeChan inChan Message.StartTabs
    liftIO $ Chan.writeChan inChan Message.StartShootings
    liftIO $ Chan.writeChan inChan Message.StartSessions
    liftIO $ Chan.writeChan inChan Message.StartDump
    liftIO $ Chan.writeChan inChan Message.StartDumpDir
    liftIO $ Chan.writeChan inChan Message.StartDagsdato
    liftIO $ Chan.writeChan inChan Message.StartCameras
    liftIO $ Chan.writeChan inChan Message.StartDoneshooting
    liftIO $ Chan.writeChan inChan Message.StartDagsdatoBackup
    liftIO $ Chan.writeChan inChan Message.StartLocation
    liftIO $ Chan.writeChan inChan Message.StartGrades
    liftIO $ Chan.writeChan inChan Message.StartBuild
    liftIO $ Chan.writeChan inChan Message.StartTranslation


receiveMessages :: forall  r m . WithChan r m => Window -> m ()
receiveMessages window = do
    outChan        <- grab @OutChan
    messages       <- liftIO $ Chan.getChanContents (unOutChan outChan)
    forM_ messages $ \x -> do
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
                mPhotographersFile <-
                    unMPhotographersFile <$> grab @MPhotographersFile
                photographersFile <- takeMVar mPhotographersFile
                runIt window photographersFile mPhotographersFile
                    `E.catchError` (\e -> do
                                       hPhotographers' <-
                                           unHPhotographers
                                               <$> grab @HPhotographers
                                       liftIO $ hPhotographers' $ Data.Failure
                                           (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       putMVar mPhotographersFile
                                               photographersFile
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
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadTabs -> do
                mTabsFile <- unMTabsFile <$> grab @MTabsFile
                tabsFile  <- takeMVar mTabsFile
                runIt2 window tabsFile mTabsFile
                    `E.catchError` (\e -> do
                                       hTabs <- unHTabs <$> grab @HTabs
                                       liftIO $ hTabs $ Data.Failure (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       putMVar mTabsFile tabsFile
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
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadShootings -> do
                mShootingsFile <- unMShootingsFile <$> grab @MShootingsFile
                shootingsFile  <- takeMVar mShootingsFile
                runIt3 window shootingsFile mShootingsFile
                    `E.catchError` (\e -> do
                                       hShootings <-
                                           unHShootings <$> grab @HShootings
                                       liftIO $ hShootings $ Data.Failure
                                           (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       putMVar mShootingsFile shootingsFile
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
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadDump -> do
                mDumpFile <- unMDumpFile <$> grab @MDumpFile
                dumpFile  <- takeMVar mDumpFile
                runIt4 window dumpFile mDumpFile
                    `E.catchError` (\e -> do
                                       hDumpDir <- unHDumpDir <$> grab @HDumpDir
                                       liftIO $ hDumpDir $ Data.Failure (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       putMVar mDumpFile dumpFile
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
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadDagsdato -> do
                mDagsdatoFile <- unMDagsdatoFile <$> grab @MDagsdatoFile
                dagsdatoFile  <- takeMVar mDagsdatoFile
                runIt5 window dagsdatoFile mDagsdatoFile
                    `E.catchError` (\e -> do
                                       hDagsdatoDir <-
                                           unHDagsdatoDir <$> grab @HDagsdatoDir
                                       liftIO $ hDagsdatoDir $ Data.Failure
                                           (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       putMVar mDagsdatoFile dagsdatoFile
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
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadCameras -> do
                mCamerasFile <- unMCamerasFile <$> grab @MCamerasFile
                camerasFile  <- takeMVar mCamerasFile
                runIt6 window camerasFile mCamerasFile
                    `E.catchError` (\e -> do
                                       hCameras <- unHCameras <$> grab @HCameras
                                       liftIO $ hCameras $ Data.Failure (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       putMVar mCamerasFile camerasFile
                                   )

--------------------------------------------------------------------------------
            Message.StopDoneshooting -> do
                return ()

            Message.WriteDoneshooting doneshooting -> do
                mDoneshootingFile <-
                    unMDoneshootingFile <$> grab @MDoneshootingFile
                doneshootingFile <- takeMVar mDoneshootingFile
                _ <- Doneshooting.writeDoneshooting doneshootingFile
                                                    doneshooting
                _ <- putMVar mDoneshootingFile doneshootingFile
                _ <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartDoneshooting -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "doneshooting"
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadDoneshooting -> do
                mDoneshootingFile <-
                    unMDoneshootingFile <$> grab @MDoneshootingFile
                doneshootingFile <- takeMVar mDoneshootingFile
                runIt7 window doneshootingFile mDoneshootingFile
                    `E.catchError` (\e -> do
                                       hDoneshootingDir <-
                                           unHDoneshootingDir
                                               <$> grab @HDoneshootingDir
                                       liftIO $ hDoneshootingDir $ Data.Failure
                                           (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       putMVar mDoneshootingFile
                                               doneshootingFile
                                   )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
            Message.StopDagsdatoBackup -> do
                return ()

            Message.WriteDagsdatoBackup dagsdatoBackup -> do
                mDagsdatoBackupFile <-
                    unMDagsdatoBackupFile <$> grab @MDagsdatoBackupFile
                dagsdatoBackupFile <- takeMVar mDagsdatoBackupFile
                _ <- DagsdatoBackup.writeDagsdatoBackup dagsdatoBackupFile
                                                        dagsdatoBackup
                _ <- putMVar mDagsdatoBackupFile dagsdatoBackupFile
                _ <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartDagsdatoBackup -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "dagsdatoBackup"
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadDagsdatoBackup -> do
                mDagsdatoBackupFile <-
                    unMDagsdatoBackupFile <$> grab @MDagsdatoBackupFile
                dagsdatoBackupFile <- takeMVar mDagsdatoBackupFile
                runIt8 window dagsdatoBackupFile mDagsdatoBackupFile
                    `E.catchError` (\e -> do
                                       hDagsdatoBackupDir <-
                                           unHDagsdatoBackupDir
                                               <$> grab @HDagsdatoBackupDir
                                       liftIO
                                           $ hDagsdatoBackupDir
                                           $ Data.Failure (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       putMVar mDagsdatoBackupFile
                                               dagsdatoBackupFile
                                   )
--------------------------------------------------------------------------------
            Message.StopSessions -> do
                return ()

            Message.WriteSessions sessions -> do
                mSessionsFile <- unMSessionsFile <$> grab @MSessionsFile
                sessionsFile  <- takeMVar mSessionsFile
                _             <- Session.writeSessions sessionsFile sessions
                _             <- putMVar mSessionsFile sessionsFile
                _             <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartSessions -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "sessions"
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadSessions -> do
                mSessionsFile <- unMSessionsFile <$> grab @MSessionsFile
                sessionsFile  <- takeMVar mSessionsFile
                runIt9 window sessionsFile mSessionsFile
                    `E.catchError` (\e -> do
                                       hSessions <-
                                           unHSessions <$> grab @HSessions
                                       liftIO $ hSessions $ Data.Failure
                                           (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       putMVar mSessionsFile sessionsFile
                                   )
--------------------------------------------------------------------------------
            Message.StopLocation -> do
                return ()

            Message.WriteLocation location -> do
                mLocationFile <- unMLocationFile <$> grab @MLocationFile
                locationFile  <- takeMVar mLocationFile
                _             <- Location.writeLocation locationFile location
                _             <- putMVar mLocationFile locationFile
                _             <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartLocation -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "location"
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadLocation -> do
                mLocationFile <- unMLocationFile <$> grab @MLocationFile
                locationFile  <- takeMVar mLocationFile
                runIt10 window locationFile mLocationFile
                    `E.catchError` (\e -> do
                                       hLocationFile <-
                                           unHLocationFile
                                               <$> grab @HLocationFile
                                       liftIO $ hLocationFile $ Data.Failure
                                           (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       putMVar mLocationFile locationFile
                                   )


--------------------------------------------------------------------------------
            Message.StopGrades -> do
                return ()

            Message.WriteGrades grades -> do
                mGradesFile <- unMGradesFile <$> grab @MGradesFile
                gradesFile  <- takeMVar mGradesFile
                _           <- Grade.writeGrades gradesFile grades
                _           <- putMVar mGradesFile gradesFile
                _           <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartGrades -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "grades"
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadGrades ->
                do
                        grades  <- readGrades
                        hGrades <- unHGrades <$> grab @HGrades
                        liftIO $ hGrades $ Data.Data grades
                    `E.catchError` (\e -> do
                                       hGrades <- unHGrades <$> grab @HGrades
                                       liftIO $ hGrades $ Data.Failure (show e)
                                   )

--------------------------------------------------------------------------------
            Message.StopDumpDir -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "dumpDir"
                _ <- liftIO $ stopMap HashMap.! key
                let newStopMap = HashMap.delete key stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.StartDumpDir -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "dumpDir"
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadDumpDir -> do
                runIt12 window `catchIOError` (\e -> do
                           traceShowM e
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF))
                    `E.catchError` (\e -> do
                                       hConfigDumpDir <-
                                           unHConfigDumpDir
                                               <$> grab @HConfigDumpDir
                                       liftIO $ hConfigDumpDir $ Data.Failure (show e)
                                       hDumpDir <- unHDumpDir <$> grab @HDumpDir
                                       liftIO $ hDumpDir $ Data.Failure (show e)
                                   )
--------------------------------------------------------------------------------
            Message.StopBuild -> do
                return ()

            Message.WriteBuild build -> do
                mBuildFile <- unMBuildFile <$> grab @MBuildFile
                buildFile  <- takeMVar mBuildFile
                _          <- Build.writeBuild buildFile build
                _          <- putMVar mBuildFile buildFile
                _          <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartBuild -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "build"
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadBuild -> do
                --mBuildFile <- unMBuildFile <$> grab @MBuildFile
                --buildFile  <- takeMVar mBuildFile
                return () 
                    {-
                runIt11 window buildFile mBuildFile
                    `E.catchError` (\e -> do
                                       hBuild <- unHBuild <$> grab @HBuild
                                       liftIO $ hBuild $ Data.Failure (show e)
                                       liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
                                       traceShowM "wtf5"
                                       putMVar mBuildFile buildFile
                                       traceShowM "wtf6"
                                   )
                                   -}

            Message.RunBuild -> do
                runBuild
                    `E.catchError` (\_ -> do
                                        return ()
                                   )

            Message.RunDownload file -> do
                { ServerDownload.runDownload file
                } `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
                    `E.catchError` (\_ -> do
                                        return ()
                                   )


            Message.RunImporter file -> do
                { ServerImport.runImport file
                } `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
                    `E.catchError` (\_ -> do
                                        return ()
                                   )

--------------------------------------------------------------------------------
            Message.StopTranslation -> do
                return ()

            Message.WriteTranslation translations -> do
                mTranslationFile <- unMTranslationFile <$> grab @MTranslationFile
                translationsFile  <- takeMVar mTranslationFile
                _           <- Translation.writeTranslations translationsFile translations
                _           <- putMVar mTranslationFile translationsFile
                _           <- liftIO $ runUI window $ do
                    flushCallBuffer -- make sure that JavaScript functions are executed
                return ()

            Message.StartTranslation -> do
                mStartMap <- unMStartMap <$> grab @(MStartMap m)
                mStopMap  <- unMStopMap <$> grab @MStopMap
                stopMap   <- takeMVar mStopMap
                startMap  <- takeMVar mStartMap
                let key = "translation"
                let watch = startMap HashMap.! key
                stop <- watch
                let newStopMap = HashMap.insert key stop stopMap
                _ <- putMVar mStartMap startMap
                _ <- putMVar mStopMap newStopMap
                return ()

            Message.ReadTranslation ->
                do
                        translations  <- readTranslation
                        hTranslations <- unHTranslationFile <$> grab @HTranslationFile
                        liftIO $ hTranslations $ translations
                    `E.catchError` (\_ -> do
                                        return ()
                                   )

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

runBuild :: forall  r m . WithChan r m => m ()
runBuild =
    ServerBuild.runBuild
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )



runIt
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt window photographersFile mPhotographersFile = do
    photographers <- getPhotographers photographersFile

    hPhotographers <- unHPhotographers <$> grab @HPhotographers

    liftIO $ hPhotographers $ Data.Data photographers
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mPhotographersFile photographersFile


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
    cameras  <- getCameras camerasFile
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
    :: (MonadIO m, MonadCatch m, WithError m)
    => FilePath
    -> m Doneshooting.Doneshooting
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
    :: (MonadIO m, MonadCatch m, WithError m)
    => FilePath
    -> m DagsdatoBackup.DagsdatoBackup
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
    sessions  <- getSessions sessionsFile
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

runIt10
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt10 window locationFile mLocationFile = do
    location  <- getLocation locationFile
    hLocation <- unHLocationFile <$> grab @HLocationFile
    liftIO $ hLocation $ Data.Data location
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mLocationFile locationFile


--type WithIOError m = MonadError AppError m
getLocation
    :: (MonadIO m, MonadCatch m, WithError m) => FilePath -> m Location.Location
getLocation fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )



runIt12 :: forall  r m . WithChan r m => Window -> m ()
runIt12 _ = do
    dumpDir        <- readDumpDir
    hConfigDumpDir <- unHConfigDumpDir <$> grab @HConfigDumpDir
    liftIO $ hConfigDumpDir $ Data.Data dumpDir



    {-
runIt13
    :: forall  r m . WithChan r m => Window -> FilePath -> MVar FilePath -> m ()
runIt13 window buildFile mBuildFile = do
    build  <- getBuild buildFile
    hBuild <- unHBuild <$> grab @HBuild
    liftIO $ hBuild $ Data.Data build
    liftIO $ runUI window flushCallBuffer -- make sure that JavaScript functions are executed
    putMVar mBuildFile buildFile

-}


--type WithIOError m = MonadError AppError m
    {-
getBuild :: (MonadIO m, MonadCatch m, WithError m) => FilePath -> m Build.Build
getBuild fp =
    readJSONFile fp
        `catchIOError` (\e -> do
                           if isUserError e
                               then E.throwError
                                   (InternalError $ ServerError (show e))
                               else E.throwError (InternalError $ WTF)
                       )
                       -}
