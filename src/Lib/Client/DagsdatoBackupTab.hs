{-# LANGUAGE RecursiveDo #-}
module Lib.Client.DagsdatoBackupTab
    ( dagsdatoBackupTab
    , DagsdatoBackupTab(..)
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
import qualified Lib.Model.DagsdatoBackup                as DagsdatoBackup
import qualified Lib.Client.Picker.Picker as Picker
import qualified Control.Lens                   as Lens
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                , (%~)
                                                , lens
                                                , view
                                                )


data DagsdatoBackupTab = DagsdatoBackupTab
    { _container :: Element
    , _selection :: Event ()
    }

instance Widget DagsdatoBackupTab where
    getElement = _container

dagsdatoBackupTab :: Behavior Translation.Translations -> Behavior Translation.Mode -> Behavior (Data.Data String DagsdatoBackup.DagsdatoBackup) -> UI DagsdatoBackupTab
dagsdatoBackupTab bTranslations bMode bDagsdatoBackup = mdo

    fallback <- Translation.translation bTranslations (pure "pick")
    let eFallback = UI.div -- Translation._translation fallback

    let display = pure $ \x -> UI.string x
    let filepath = fmap (Lens.view DagsdatoBackup.unDagsdatoBackup ) <$> bDagsdatoBackup
    picker <- Picker.picker bTranslations bMode filepath display (pure $ \_ -> eFallback) -- (element fallback))

    _container <- UI.div #+ [element picker]
    let _selection = Picker._selection picker

    return DagsdatoBackupTab {..}
