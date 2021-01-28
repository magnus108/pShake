{-# LANGUAGE RecursiveDo #-}
module Lib.Client.DagsdatoTab
    ( dagsdatoTab
    , DagsdatoTab(..)
    )
where
import qualified Lib.Client.Translation.Translation as Translation
import qualified Data.HashMap.Strict           as HashMap

import qualified Lib.Model.Data                as Data
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                , (%~)
                                                , lens
                                                , view
                                                )

import qualified Relude.Unsafe as Unsafe
import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Lib.Model.Dagsdato                as Dagsdato
import qualified Lib.Client.Picker.Picker as Picker
import qualified Control.Lens                   as Lens
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                , (%~)
                                                , lens
                                                , view
                                                )


data DagsdatoTab = DagsdatoTab
    { _container :: Element
    , _selection :: Event ()
    }

instance Widget DagsdatoTab where
    getElement = _container

dagsdatoTab :: Behavior Translation.Translations -> Behavior Translation.Mode -> Behavior (Data.Data String Dagsdato.Dagsdato) -> UI DagsdatoTab
dagsdatoTab bTranslations bMode bDagsdato = mdo

    fallback <- Translation.translation bTranslations bMode (pure "pick")
    let eFallback = Translation._translation fallback

    let display = pure $ \x -> UI.string x
    let filepath = fmap (Lens.view Dagsdato.unDagsdato ) <$> bDagsdato
    picker <- Picker.picker filepath display (pure (element fallback))

    _container <- UI.div #+ [element picker]
    let _selection = Picker._selection picker

    return DagsdatoTab {..}
