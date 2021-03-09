{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
module Lib.Client.InsertPhotographeesTab
    ( insertPhotographeesTab
    , InsertPhotographeesTab(..)
    )
where




import           Control.Lens                   ( (^.))
import qualified Relude.Unsafe                 as Unsafe
import Lib.Client.Utils
import           Utils.Comonad
import           Control.Conditional            ( (?<>) )
import qualified Control.Lens                  as Lens
import qualified Lib.Model.Translation         as Translation
import qualified Lib.Client.Translation.Translation
                                               as ClientTranslation

import qualified Lib.Client.Input.Text as Text
import qualified Utils.ListZipper              as ListZipper

import qualified Lib.Model.Data                as Data
import qualified Lib.Model.Grade                as Grade
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI


data InsertPhotographeesTab = InsertPhotographeesTab
    { _container :: Element
    , _selection :: !(Event Grade.Grades)
    , _gradeInsert :: !(Event Grade.Grades)
    }

instance Widget InsertPhotographeesTab where
    getElement = _container


data Mode
    = Closed
    | Open
    deriving (Eq, Show)

switch :: Mode -> Mode
switch Open = Closed
switch Closed = Open


insertPhotographeesTab
    :: Behavior Translation.Translations
    -> Behavior ClientTranslation.Mode
    -> Behavior [UI Element]
    -> Behavior [UI Element]
    -> Behavior [UI Element]
    -> Behavior (Element -> [UI Element])
    -> Behavior (Data.Data String Grade.Grades)
    -> UI InsertPhotographeesTab
insertPhotographeesTab bTranslations bTransMode errorView loadingView notAskedView insertView bGrades = mdo


    (eSelection, hSelection) <- liftIO $ newEvent
    (ePopup , hPopup      ) <- liftIO $ newEvent

    let eSwitch = switch <$> Unsafe.head <$> unions
            [ bDropMode <@ eSelection
            , bDropMode <@ ePopup
            ]

    bDropMode <- stepper Closed $ eSwitch

    let bZipper = Lens.view Grade.unGrades <<$>> bGrades


    let bDisplayOpen = pure $ \center grades -> do
                text' <- UI.span # set text (extract grades ^. Grade.gradeId)
                display <- UI.button
                    #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                    #+ fmap element [text']

                UI.on UI.click display $ \_ -> do
                    liftIO $ hSelection (Data.Data grades)

                return display


    _insertGrade <- UI.button #. "button"
    let bDisplayClosed = insertView <&> \gg grades -> do
                    text' <- UI.span # set text (extract grades ^. Grade.gradeId)
                    icon <-
                        UI.span #. "icon" #+ [UI.mkElement "i" #. "fas fa-caret-down"]
                    display <- UI.button
                        #. "button"
                        #+ fmap element [text', icon]

                    display'' <- UI.div #+ (element display : gg _insertGrade)

                    UI.on UI.click display $ \_ -> do
                        liftIO $ hPopup ()

                    return display''





    let finalDisplay = do
            displayOpen <- bDisplayOpen
            dropMode <- bDropMode
            displayClosed <- bDisplayClosed
            return $ \zipper ->
                case dropMode of
                    Open -> do
                        [UI.div #. "buttons has-addons" #+ ListZipper.toList (ListZipper.bextend displayOpen zipper)]
                    Closed -> do
                        [displayClosed zipper]


    
    let _gradeInsert = filterJust $ mkNewGrade <$> (Data.toJust <$> bGrades) <@  UI.click _insertGrade

    let _selection = fmap Grade.Grades $ filterJust $ Data.toJust <$> eSelection

    _container <- UI.div # sink items (Data.data'' <$> loadingView <*> notAskedView <*> errorView <*> finalDisplay <*> bZipper)

    return InsertPhotographeesTab { .. }


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

