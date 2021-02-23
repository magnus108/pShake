{-# LANGUAGE RecursiveDo #-}
module Lib.Client.DoneshootingTab
    ( doneshootingTab
    , DoneshootingTab(..)
    )
where
import qualified Lib.Model.Translation         as Translation
import qualified Lib.Client.Translation.Translation
                                               as ClientTranslation

import qualified Control.Lens                  as Lens
import qualified Lib.Model.Data                as Data

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Lib.Model.Doneshooting        as Doneshooting
import qualified Lib.Client.Picker.Picker      as Picker


data DoneshootingTab = DoneshootingTab
    { _container :: Element
    , _selection :: Event ()
    }

instance Widget DoneshootingTab where
    getElement = _container

doneshootingTab
    :: Behavior Translation.Translations
    -> Behavior ClientTranslation.Mode
    -> Behavior (Data.Data String Doneshooting.Doneshooting)
    -> UI DoneshootingTab
doneshootingTab bTranslations bMode bDoneshooting = mdo

    --fallback <- ClientTranslation.translation bTranslations (pure "pick")
    let eFallback = UI.div -- Translation._translation fallback

    let display   = pure $ \x -> UI.string x
    let filepath =
            fmap (Lens.view Doneshooting.unDoneshooting) <$> bDoneshooting
    picker <- Picker.picker bTranslations
                            bMode
                            filepath
                            display
                            (pure $ \_ -> eFallback)--(element fallback))

    _container <- UI.div #+ [element picker]
    let _selection = Picker._selection picker

    return DoneshootingTab { .. }
