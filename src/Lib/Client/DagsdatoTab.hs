{-# LANGUAGE RecursiveDo #-}
module Lib.Client.DagsdatoTab
    ( dagsdatoTab
    , DagsdatoTab(..)
    )
where
import qualified Lib.Model.Translation         as Translation
import qualified Lib.Client.Translation.Translation
                                               as ClientTranslation

import qualified Lib.Model.Data                as Data

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Lib.Model.Dagsdato            as Dagsdato
import qualified Lib.Client.Picker.Picker      as Picker
import qualified Control.Lens                  as Lens


data DagsdatoTab = DagsdatoTab
    { _container :: Element
    , _selection :: Event ()
    }

instance Widget DagsdatoTab where
    getElement = _container

dagsdatoTab
    :: Behavior Translation.Translations
    -> Behavior ClientTranslation.Mode
    -> Behavior (Data.Data String Dagsdato.Dagsdato)
    -> UI DagsdatoTab
dagsdatoTab bTranslations bMode bDagsdato = mdo

    --fallback <- ClientTranslation.translation bTranslations (pure "pick")
    let eFallback = UI.div --Translation._translation fallback

    let bDisplay  = pure $ \x -> UI.string x
    let bFilepath = fmap (Lens.view Dagsdato.unDagsdato) <$> bDagsdato
    picker <- Picker.picker bTranslations
                            bMode
                            bFilepath
                            bDisplay
                            (pure $ \_ -> eFallback) --(element fallback))

    _container <- UI.div #+ [element picker]
    let _selection = Picker._selection picker

    return DagsdatoTab { .. }
