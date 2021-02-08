{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Translation.Translation2
    ( Translations
    , Mode(..)
    , toggle
    , translation2
    )
where

import qualified Lib.Model.Data                as Data
import qualified Lib.Client.Input.Text as Entry
import qualified Lib.Client.Pop.Popup2         as Popup

import qualified Relude.Unsafe                 as Unsafe
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                , (%~)
                                                , lens
                                                , view
                                                )

import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI


import qualified Data.HashMap.Strict           as HashMap



data Mode
    = Translating
    | Normal
    deriving (Eq, Show)

toggle :: Mode -> Mode
toggle Translating = Normal
toggle Normal      = Translating

type Translations = HashMap String String


translation2
    :: Behavior Translations
    -> Behavior Mode
    -> Behavior String
    -> UI ((Element, Element, Element, Entry.TextEntry), Tidings Popup.Mode)
translation2 bTranslations bMode bKey = mdo

    ((close, open), tPopup) <- Popup.popup2 bOpen bClose

    text' <- UI.span # sink text bText
    input <- Entry.entry bTranslate

    let bText = (\k -> HashMap.lookupDefault k k) <$> bKey <*> bTranslations
    let bOpen  = (\s -> ("{{" ++ s ++ "}}")) <$> bKey
    let bClose = HashMap.lookupDefault "close" "close" <$> bTranslations
    let bTranslate = HashMap.lookupDefault "" <$> bKey <*> bTranslations

    return ((text', close, open, input), tPopup)
