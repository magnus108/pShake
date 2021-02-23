{-# LANGUAGE RecursiveDo #-}
module Lib.Client.SessionsTab
    ( sessionsTab
    , SessionsTab(..)
    )
where

import qualified Lib.Model.Session             as Session
import qualified Lib.Model.Translation         as Translation
import qualified Lib.Client.Translation.Translation
                                               as ClientTranslation

import qualified Lib.Model.Data                as Data
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI


data SessionsTab = SessionsTab
    { _container :: Element
    , _selection :: Event Session.Sessions
    }

instance Widget SessionsTab where
    getElement = _container


sessionsTab
    :: Behavior Translation.Translations
    -> Behavior ClientTranslation.Mode
    -> Behavior (Data.Data String Session.Sessions)
    -> UI SessionsTab
sessionsTab _ _ _ = mdo

    {-
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
        -}

    let _selection = UI.never
            {-
            filterJust
                $   Data.toJust
                <$> fmap Session.Sessions
                <$> (Dropdown._selection sessions)
                -}
    _container <- UI.div --element sessions

    return SessionsTab { .. }
