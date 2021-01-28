{-# LANGUAGE RecursiveDo #-}
module Lib.Client.CamerasTab
    ( camerasTab
    , CamerasTab(..)
    )
where

import Control.Conditional ((?<>))
import qualified Lib.Model.Camera        as Camera
import qualified Lib.Client.Select.Dropdown    as Dropdown
import qualified Lib.Client.Translation.Translation
                                               as Translation
import qualified Data.HashMap.Strict           as HashMap

import qualified Lib.Model.Data                as Data
import qualified Relude.Unsafe                 as Unsafe
import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Lib.Model.Dump                as Dump
import qualified Lib.Client.Picker.Picker      as Picker
import qualified Control.Lens                  as Lens
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                , (%~)
                                                , lens
                                                , view
                                                )


data CamerasTab = CamerasTab
    { _container :: Element
    , _selection :: Event Camera.Cameras
    }

instance Widget CamerasTab where
    getElement = _container


camerasTab
    :: Behavior Translation.Translations
    -> Behavior Translation.Mode
    -> Behavior (Data.Data String Camera.Cameras)
    -> UI CamerasTab
camerasTab bTranslations bMode bCameras = mdo

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

    let _selection =
            filterJust
                $   Data.toJust
                <$> fmap Camera.Cameras
                <$> (Dropdown._selection cameras)
    _container <- element cameras

    return CamerasTab { .. }
