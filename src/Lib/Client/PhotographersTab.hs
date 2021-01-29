{-# LANGUAGE RecursiveDo #-}
module Lib.Client.PhotographersTab
    ( photographersTab
    , PhotographersTab(..)
    )
where
import Control.Conditional ((?<>))
import qualified Lib.Model.Photographer        as Photographer
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


data PhotographersTab = PhotographersTab
    { _container :: Element
    , _selection :: Event Photographer.Photographers
    }

instance Widget PhotographersTab where
    getElement = _container


photographersTab
    :: Behavior Translation.Translations
    -> Behavior Translation.Mode
    -> Behavior (Data.Data String Photographer.Photographers)
    -> UI PhotographersTab
photographersTab bTranslations bMode bPhotographers = mdo

    let
        bDisplay = pure $ \s b x -> do
            text <- UI.span # set text (x ^. Photographer.name)
            icon <-
                UI.span #. "icon" #+ [UI.mkElement "i" #. "fas fa-caret-down"]
            UI.button
                #. (b ?<> "is-info is-seleceted" <> " " <> "button")
                #+ fmap element ([text] <> not s ?<> [icon])

    photographers <- Dropdown.dropdown
        (fmap (Lens.view Photographer.unPhotographers) <$> bPhotographers)
        bDisplay

    let _selection =
            filterJust
                $   Data.toJust
                <$> fmap Photographer.Photographers
                <$> (Dropdown._selection photographers)
    _container <- element photographers

    return PhotographersTab { .. }