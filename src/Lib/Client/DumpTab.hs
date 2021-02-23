{-# LANGUAGE RecursiveDo #-}
module Lib.Client.DumpTab
    ( dumpTab
    , DumpTab(..)
    )
where

import qualified Lib.Model.Translation         as Translation
import qualified Lib.Client.Translation.Translation
                                               as ClientTranslation

import qualified Lib.Model.Data                as Data

import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI
import qualified Lib.Model.Dump                as Dump
import qualified Lib.Client.Picker.Picker      as Picker
import qualified Control.Lens                  as Lens


data DumpTab = DumpTab
    { _container :: Element
    , _selection :: Event ()
    }

instance Widget DumpTab where
    getElement = _container

dumpTab
    :: Behavior Translation.Translations
    -> Behavior ClientTranslation.Mode
    -> Behavior (Data.Data String Dump.Dump)
    -> UI DumpTab
dumpTab bTranslations bMode bDump = mdo

    --fallback <- ClientTranslation.translation bTranslations (pure "pick")
    let eFallback = UI.div --Translation._translation fallback

    let display   = pure $ \x -> UI.string x
    let filepath = fmap (Lens.view Dump.unDump) <$> bDump
    picker <- Picker.picker bTranslations
                            bMode
                            filepath
                            display
                            (pure $ \_ -> eFallback) --(element fallback))

    _container <- UI.div #+ [element picker]
    let _selection = Picker._selection picker

    return DumpTab { .. }
