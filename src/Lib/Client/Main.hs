{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Main
    ( mainTab
    , MainTab(..)
    )
where

import Data.Char

import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                , (%~)
                                                , lens
                                                , view
                                                , Lens'
                                                )
import qualified Control.Lens                  as Lens

import qualified Relude.Unsafe                 as Unsafe
import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI

import qualified Lib.Model.Build             as Build
import qualified Lib.Model.DumpDir             as DumpDir
import qualified Lib.Model.Grade               as Grade
import qualified Lib.Model.Data                as Data

import qualified Utils.ListZipper              as ListZipper
import           Utils.Comonad

data Count a = Count
    { _elementCount :: Element
    }

instance Widget (Count a) where
    getElement = _elementCount

countable :: String -> Behavior (Data.Data String [x]) -> UI (Count a)
countable str bItems = do

    _elementCount <- UI.div

    element _elementCount # sink (count str) bItems

    return Count { .. }

count str = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            return x # set text (str ++ " " ++ (show (length item)))


-----------------------------------------------------------------------------
data GradesSelect = GradesSelect
    { _elementGradesSelect   :: Element
    , _gradesSelect :: !(Event Grade.Grades)
    }

instance Widget GradesSelect where
    getElement = _elementGradesSelect

gradesSelect :: Behavior (Data.Data String Grade.Grades) -> UI GradesSelect
gradesSelect bItems = do
    _elementGradesSelect <- UI.div
    list                 <- UI.select

    element _elementGradesSelect # sink (select list) (fmap mkGrades <$> bItems)

    let _gradesSelect =
            filterJust
                $   om selectGrade
                <$> (Data.toJust <$> bItems)
                <@> (filterJust (UI.selectionChange list))

    return GradesSelect { .. }

select list = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            element list # set children [] #+ item
            return x # set children [] #+ [element list]


selectGrade :: Grade.Grades -> Int -> Maybe Grade.Grades
selectGrade grades selected = asum $ ListZipper.toNonEmpty $ ListZipper.iextend
    (\thisIndex grades'' -> if selected == thisIndex
        then Just (Grade.Grades grades'')
        else Nothing
    )
    (Grade._unGrades grades)


mkGrades :: Grade.Grades -> [UI Element]
mkGrades grades' = do
    let zipper = Grade._unGrades grades'
    let elems = ListZipper.iextend
            (\i zipper'' -> (i, zipper == zipper'', extract zipper''))
            zipper
    fmap mkGrade (ListZipper.toList elems)


mkGrade :: (Int, Bool, Grade.Grade) -> UI Element
mkGrade (thisIndex, isCenter, grade) = do
    let name   = grade ^. Grade.gradeId -- change 
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then option # set UI.selected True else option

-----------------------------------------------------------------------------
data PhotographeesSelect = PhotographeesSelect
    { _elementPhotographeesSelect   :: Element
    , _photographeesSelect :: !(Event Grade.Photographees)
    }

instance Widget PhotographeesSelect where
    getElement = _elementPhotographeesSelect


photographeesSelect
    :: Behavior (Data.Data String Grade.Photographees) -> UI PhotographeesSelect
photographeesSelect bItems = do
    _elementPhotographeesSelect <- UI.div
    list                        <- UI.select

    element _elementPhotographeesSelect
        # sink (select list) (fmap mkPhotographees <$> bItems)

    let _photographeesSelect =
            filterJust
                $   om selectPhotographee
                <$> (Data.toJust <$> bItems)
                <@> (filterJust (UI.selectionChange list))

    return PhotographeesSelect { .. }

selectPhotographee :: Grade.Photographees -> Int -> Maybe Grade.Photographees
selectPhotographee photographees selected =
    asum $ ListZipper.toNonEmpty $ ListZipper.iextend
        (\thisIndex photographees'' -> if selected == thisIndex
            then Just (Grade.Photographees photographees'')
            else Nothing
        )
        (Grade._unPhotographees photographees)


mkPhotographees :: Grade.Photographees -> [UI Element]
mkPhotographees photographees' = do
    let zipper = Grade._unPhotographees photographees'
    let elems = ListZipper.iextend
            (\i zipper'' -> (i, zipper == zipper'', extract zipper''))
            zipper
    fmap mkPhotographee (ListZipper.toList elems)


mkPhotographee :: (Int, Bool, Grade.Photographee) -> UI Element
mkPhotographee (thisIndex, isCenter, photographee) = do
    let name   = photographee ^. Grade.name -- change 
    let option = UI.option # set value (show thisIndex) # set text name
    if isCenter then option # set UI.selected True else option
-------------------------------------------------------------------------------
data TextEntry = TextEntry
    { _elementTE :: Element
    , _userTE    :: Tidings String
    }

instance Widget TextEntry where
    getElement = _elementTE

userText :: TextEntry -> Tidings String
userText = _userTE

entry :: Behavior String -> UI TextEntry
entry bValue = do
    input    <- UI.input

    bEditing <- stepper False $ and <$> unions
        [True <$ UI.focus input, False <$ UI.blur input]

    window <- askWindow
    liftIOLater $ Reactive.onChange bValue $ \s -> runUI window $ do
        editing <- liftIO $ currentValue bEditing
        when (not editing) $ void $ element input # set value s

    let _elementTE = input
        _userTE    = tidings bValue $ UI.valueChange input

    return TextEntry { .. }
-------------------------------------------------------------------------------
data SearchEntry a = SearchEntry
    { _elementSearch :: Element
    , _search    :: Event (Maybe (ListZipper.ListZipper a))
    }

instance Widget (SearchEntry a) where
    getElement = _elementSearch

search :: (a -> String) -> Behavior (Data.Data String (ListZipper.ListZipper a)) -> UI (SearchEntry a)
search f bValue = mdo

    input <- entry bSearchString

    bSearchString <- stepper "" . rumors $ userText input
    
    let eSearchString = rumors $ fmap toUpper <$> userText input

    let eSearch = (\b s -> b >>= (ListZipper.findFirst ((==) s . f)))
                <$> (Data.toJust <$> bValue)
                <@> eSearchString

    _elementSearch <- element input
    let _search    = eSearch

    return SearchEntry { .. }

-------------------------------------------------------------------------------
data BuildSection = BuildSection
    { _elementBuild :: Element
    , _build    :: Event ()
    }

instance Widget BuildSection where
    getElement = _elementBuild

buildSection :: Behavior (Data.Data String Build.Build) -> UI BuildSection
buildSection bValue = do
    content <- UI.div
    button <- UI.button # set text "byg"

    _elementBuild <- element content #+ [element button]
    let _build = UI.click button

    return BuildSection { .. }

-------------------------------------------------------------------------------

data MainTab = MainTab
    { _elementMainTab :: Element
    , _eMainTab :: !(Event Grade.Grades)
    , _eBuild :: !(Event ())
    }

instance Widget MainTab where
    getElement = _elementMainTab


mainTab
    :: Behavior (Data.Data String Grade.Grades)
    -> Behavior (Data.Data String DumpDir.DumpDir)
    -> Behavior (Data.Data String Build.Build)
    -> UI MainTab
mainTab bGrades bDumpDir bBuild = do

    _elementMainTab <- UI.div

    let bPhotographees =
            fmap (view (Grade.unGrades . ListZipper.zipperL . Grade.photographees))
                <$> bGrades

    elemBuild <- buildSection bBuild
    elemPhotographees <- photographeesSelect bPhotographees
    elemGrades        <- gradesSelect bGrades
    elemDumpDirCount  <- countable "Antal filer i dump:"
                                   (fmap (Lens.view DumpDir.unDumpDir) <$> bDumpDir)
    elemPhotographeesCount <- countable
        "Elever i klasse:"
        (   fmap ListZipper.toList
        <$> fmap (view Grade.unPhotographees)
        <$> bPhotographees
        )

    elemSearch <- search (view Grade.tid)
        (fmap (view Grade.unPhotographees) <$> bPhotographees)

    let ePhotographeesSelect =
            filterJust
                $   flip (fmap . setPhotographees)
                <$> (Data.toJust <$> bGrades)
                <@> _photographeesSelect elemPhotographees

    let eGradesSelect = _gradesSelect elemGrades

    let eSearch =
            filterJust
                $   flip (fmap . setPhotographees)
                <$> (Data.toJust <$> bGrades)
                <@> (Grade.Photographees <$> (filterJust (_search elemSearch)))

    let _eMainTab =
            Unsafe.head <$> unions [ePhotographeesSelect, eGradesSelect, eSearch]

    let _eBuild = _build elemBuild

    element _elementMainTab
        #+ [ element elemGrades
           , element elemPhotographees
           , element elemPhotographeesCount
           , element elemDumpDirCount
           , element elemSearch
           , element elemBuild
           ]

    return MainTab { .. }



setPhotographees :: Grade.Photographees -> Grade.Grades -> Grade.Grades
setPhotographees photographees grades =
    Grade.unGrades . ListZipper.zipperL . Grade.photographees .~ photographees $ grades
