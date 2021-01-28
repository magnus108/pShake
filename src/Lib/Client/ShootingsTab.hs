{-# LANGUAGE RecursiveDo #-}
module Lib.Client.ShootingsTab
    ( shootingsTab
    , ShootingsTab(..)
    )
where

import Control.Conditional ((?<>))
import qualified Lib.Model.Shooting        as Shooting
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


data ShootingsTab = ShootingsTab
    { _container :: Element
    , _selection :: Event Shooting.Shootings
    }

instance Widget ShootingsTab where
    getElement = _container


shootingsTab
    :: Behavior Translation.Translations
    -> Behavior Translation.Mode
    -> Behavior (Data.Data String Shooting.Shootings)
    -> UI ShootingsTab
shootingsTab bTranslations bMode bShootings = mdo

    let
        bDisplay = pure $ \s b x -> do
            text <- UI.span # set text (show x)
            icon <-
                UI.span #. "icon" #+ [UI.mkElement "i" #. "fas fa-caret-down"]
            UI.button
                #. (b ?<> "is-info is-seleceted" <> " " <> "button")
                #+ fmap element ([text] <> not s ?<> [icon])

    shootings <- Dropdown.dropdown
        (fmap (Lens.view Shooting.unShootings) <$> bShootings)
        bDisplay

    let _selection =
            filterJust
                $   Data.toJust
                <$> fmap Shooting.Shootings
                <$> (Dropdown._selection shootings)
    _container <- element shootings

    return ShootingsTab { .. }
