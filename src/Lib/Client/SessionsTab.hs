{-# LANGUAGE RecursiveDo #-}
module Lib.Client.SessionsTab
    ( sessionsTab
    , SessionsTab(..)
    )
where

import Control.Conditional ((?<>))
import qualified Lib.Model.Session        as Session
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


data SessionsTab = SessionsTab
    { _container :: Element
    , _selection :: Event Session.Sessions
    }

instance Widget SessionsTab where
    getElement = _container


sessionsTab
    :: Behavior Translation.Translations
    -> Behavior Translation.Mode
    -> Behavior (Data.Data String Session.Sessions)
    -> UI SessionsTab
sessionsTab bTranslations bMode bSessions = mdo

    let
        bDisplay = pure $ \s b x -> do
            text <- UI.span # set text (show x)
            icon <-
                UI.span #. "icon" #+ [UI.mkElement "i" #. "fas fa-caret-down"]
            UI.button
                #. (b ?<> "is-info is-seleceted" <> " " <> "button")
                #+ fmap element ([text] <> not s ?<> [icon])

    sessions <- Dropdown.dropdown
        (fmap (Lens.view Session.unSessions) <$> bSessions)
        bDisplay

    let _selection =
            filterJust
                $   Data.toJust
                <$> fmap Session.Sessions
                <$> (Dropdown._selection sessions)
    _container <- element sessions

    return SessionsTab { .. }
