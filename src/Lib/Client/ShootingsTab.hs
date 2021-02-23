{-# LANGUAGE RecursiveDo #-}
module Lib.Client.ShootingsTab
    ( shootingsTab
    , ShootingsTab(..)
    )
where

import qualified Lib.Model.Shooting            as Shooting


import qualified Lib.Model.Translation         as Translation
import qualified Lib.Client.Translation.Translation
                                               as ClientTranslation


import qualified Lib.Model.Data                as Data
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI


data ShootingsTab = ShootingsTab
    { _container :: Element
    , _selection :: Event Shooting.Shootings
    }

instance Widget ShootingsTab where
    getElement = _container


shootingsTab
    :: Behavior Translation.Translations
    -> Behavior ClientTranslation.Mode
    -> Behavior (Data.Data String Shooting.Shootings)
    -> UI ShootingsTab
shootingsTab _ _ _ = mdo

    {-
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
        -}

    let _selection = UI.never
            --filterJust
             --   $   Data.toJust
              --  <$> fmap Shooting.Shootings
               -- <$> (Dropdown._selection shootings)
    _container <- UI.div --element shootings

    return ShootingsTab { .. }
