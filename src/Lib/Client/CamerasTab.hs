{-# LANGUAGE RecursiveDo #-}
module Lib.Client.CamerasTab
    ( camerasTab
    , CamerasTab(..)
    )
where

import qualified Lib.Model.Camera        as Camera
import qualified Lib.Model.Translation                as Translation
import qualified Lib.Client.Translation.Translation as ClientTranslation

import qualified Lib.Model.Data                as Data
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI


data CamerasTab = CamerasTab
    { _container :: Element
    , _selection :: Event Camera.Cameras
    }

instance Widget CamerasTab where
    getElement = _container


camerasTab
    :: Behavior Translation.Translations
    -> Behavior ClientTranslation.Mode
    -> Behavior (Data.Data String Camera.Cameras)
    -> UI CamerasTab
camerasTab _ _ _ = mdo

    {-
    let
        bDisplay = pure $ \s b x -> do
            text <- UI.span # set text (show x)
            icon <-
                UI.span #. "icon" #+ [UI.mkElement "i" #. "fas fa-caret-down"]
            UI.button
                #. (b ?<> "is-info is-seleceted" <> " " <> "button")
                #+ fmap element ([text] <> not s ?<> [icon])

    cameras <- Dropdown.dropdown
        (fmap (Lens.view Camera.unCameras) <$> bCameras)
        bDisplay
        -}

    let _selection = UI.never
            {-
            filterJust
                $   Data.toJust
                <$> fmap Camera.Cameras
                <$> (Dropdown._selection cameras)
                -}
    _container <- UI.div --element cameras

    return CamerasTab { .. }
