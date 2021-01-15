{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Main
    ( dumpCount
    , mainTab
    , MainTab(..)
    )
where
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                )

import qualified Relude.Unsafe                 as Unsafe
import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI

import qualified Lib.Model.DumpDir             as DumpDir
import qualified Lib.Model.Grade               as Grade
import qualified Lib.Model.Data                as Data

import qualified Utils.ListZipper              as ListZipper
import           Utils.Comonad

data Count = Count
    { _elementCount :: Element
    }

instance Widget Count where
    getElement = _elementCount

dumpCount :: Behavior (Data.Data String DumpDir.DumpDir) -> UI Count
dumpCount bItems = do

    _elementCount <- UI.div

    element _elementCount # sink count bItems

    return Count { .. }

count = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            return x # set text (show (length (DumpDir.unDumpDir item)))


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
    (Grade.unGrades grades)


mkGrades :: Grade.Grades -> [UI Element]
mkGrades grades' = do
    let zipper = Grade.unGrades grades'
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
        (Grade.unPhotographees photographees)


mkPhotographees :: Grade.Photographees -> [UI Element]
mkPhotographees photographees' = do
    let zipper = Grade.unPhotographees photographees'
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


data MainTab = MainTab
    { _elementMainTab :: Element
    , _eMainTab :: !(Event Grade.Grades)
    }

instance Widget MainTab where
    getElement = _elementMainTab


mainTab
    :: Behavior (Data.Data String Grade.Grades)
    -> Behavior (Data.Data String DumpDir.DumpDir)
    -> UI MainTab
mainTab bGrades bDumpDir = do
    _elementMainTab <- UI.div
    let bPhotographees =
            fmap (\x -> extract (Grade.unGrades x) ^. Grade.photographees)
                <$> bGrades

    elemPhotographees <- photographeesSelect bPhotographees
    elemGrades        <- gradesSelect bGrades
    elemDumpDirCount  <- dumpCount bDumpDir

    let ePhotographeesSelect =
            filterJust
                $   flip (fmap . setPhotographees)
                <$> (Data.toJust <$> bGrades)
                <@> _photographeesSelect elemPhotographees

    let eGradesSelect = _gradesSelect elemGrades

    let _eMainTab =
            Unsafe.head <$> unions [ePhotographeesSelect, eGradesSelect]

    element _elementMainTab
        #+ [ element elemPhotographees
           , element elemGrades
           , element elemDumpDirCount
           ]

    return MainTab { .. }


setPhotographees :: Grade.Photographees -> Grade.Grades -> Grade.Grades
setPhotographees photographees grades =
    let ListZipper.ListZipper ls x rs = Grade.unGrades grades
    in  Grade.Grades $ ListZipper.ListZipper
            ls
            (x & Grade.photographees .~ photographees)
            rs
