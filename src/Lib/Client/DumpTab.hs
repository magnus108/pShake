{-# LANGUAGE RecursiveDo #-}
module Lib.Client.DumpTab
    ( dumpTab
    , DumpTab(..)
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
import qualified Lib.Model.Dump                as Dump
import qualified Lib.Client.Picker.Picker as Picker
import qualified Control.Lens                   as Lens
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                , (%~)
                                                , lens
                                                , view
                                                )


data DumpTab = DumpTab
    { _container :: Element
    , _selection :: Event ()
    }

instance Widget DumpTab where
    getElement = _container

dumpTab :: Behavior Translation.Translations -> Behavior Translation.Mode -> Behavior (Data.Data String Dump.Dump) -> UI DumpTab
dumpTab bTranslations bMode bDump = mdo

    fallback <- Translation.translation bTranslations (pure "pick")
    let eFallback = UI.div --Translation._translation fallback

    let display = pure $ \x -> UI.string x
    let filepath = fmap (Lens.view Dump.unDump) <$> bDump
    picker <- Picker.picker bTranslations bMode filepath display (pure $ \_ -> eFallback) --(element fallback))

    _container <- UI.div #+ [element picker]
    let _selection = Picker._selection picker

    return DumpTab {..}
