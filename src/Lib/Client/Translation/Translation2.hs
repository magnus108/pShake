{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Translation.Translation2
    ( translation
    , Translations
    , Mode(..)
    , toggle
    )
where

import           Lib.Client.Input.Text
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


translation
    :: Tidings Translations
    -> Tidings Mode
    -> Behavior String
    -> UI ((Element, Element, Element), Tidings Element, Tidings String)
translation tTranslations tMode bKey = mdo

    let bTranslations = facts tTranslations

    let bValue = (\k -> HashMap.lookupDefault k k) <$> bKey <*> bTranslations
    _text <- UI.span # sink text bValue

    let bOpen  = (\s -> "{{" ++ s ++ "}}") <$> bKey
    let bClose = HashMap.lookupDefault "close" "close" <$> bTranslations -- this wrong?
    ((_open, _close), tPopup) <- Popup.popup bOpen bClose

    let bToTranslate = HashMap.lookupDefault "" <$> bKey <*> bTranslations
    _translationInput <- entry bToTranslate
    _popup <- UI.div #+ [element _translationInput, element _close]

    let _translation = userText _translationInput

    let tToggle =
            (\popup mode -> if popup == Popup.Open && mode == Translating
                    then _popup
                    else if popup == Popup.Closed then _open else _text
                )
                <$> tPopup
                <*> tMode

    return ((_text, _open, _popup), tToggle, _translation)
