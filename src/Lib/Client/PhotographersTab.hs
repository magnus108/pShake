{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
module Lib.Client.PhotographersTab
    ( photographersTab
    , PhotographersTab(..)
    )
where
import           Utils.Comonad
import Lib.Client.Utils

import qualified Utils.ListZipper              as ListZipper
import           Control.Conditional            ( (?<>) )
import qualified Lib.Model.Photographer        as Photographer


import qualified Lib.Model.Data                as Data
import qualified Relude.Unsafe                 as Unsafe
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Control.Lens                  as Lens
import           Control.Lens                   ( (^.))


data PhotographersTab = PhotographersTab
    { _container :: Element
    , _selection :: Event Photographer.Photographers
    }

instance Widget PhotographersTab where
    getElement = _container


data Mode
    = Closed
    | Open
    deriving (Eq, Show)

switch :: Mode -> Mode
switch Open = Closed
switch Closed = Open


photographersTab :: Behavior [UI Element] -> Behavior [UI Element] -> Behavior [UI Element] -> Behavior (Data.Data String Photographer.Photographers) -> UI PhotographersTab
photographersTab errorView loadingView notAskedView bPhotographers = mdo

    (eSelection, hSelection) <- liftIO $ newEvent
    (ePopup , hPopup      ) <- liftIO $ newEvent

    let eSwitch = switch <$> Unsafe.head <$> unions
            [ bDropMode <@ eSelection
            , bDropMode <@ ePopup
            ]

    bDropMode <- stepper Closed $ eSwitch

    let bZipper = Lens.view Photographer.unPhotographers <<$>> bPhotographers

    let bDisplayOpen = pure $ \center photographers -> do
                text' <- UI.span # set text (extract photographers ^. Photographer.name)
                display <- UI.button
                    #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                    #+ fmap element [text']

                UI.on UI.click display $ \_ -> do
                    liftIO $ hSelection (Data.Data photographers)

                return display


    let bDisplayClosed = pure $ \photographers -> do
                    text' <- UI.span # set text (extract photographers ^. Photographer.name)
                    icon <-
                        UI.span #. "icon" #+ [UI.mkElement "i" #. "fas fa-caret-down"]
                    display <- UI.button
                        #. "button"
                        #+ fmap element [text', icon]

                    UI.on UI.click display $ \_ -> do
                        liftIO $ hPopup ()

                    return display

    let finalDisplay = do
            displayOpen <- bDisplayOpen
            displayClosed <- bDisplayClosed
            dropMode <- bDropMode
            return $ \ zipper ->
                case dropMode of
                    Open -> do
                        [UI.div #. "buttons has-addons" #+ ListZipper.toList (ListZipper.bextend displayOpen zipper)]
                    Closed -> do
                        [displayClosed zipper]


    _container <- UI.div # sink items (Data.data'' <$> loadingView <*> notAskedView <*> errorView <*> finalDisplay <*> bZipper)

    let _selection = fmap Photographer.Photographers $ filterJust $ Data.toJust <$> eSelection

    return PhotographersTab { .. }
