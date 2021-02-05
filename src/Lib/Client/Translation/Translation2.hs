{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Translation.Translation2
    ( translation
    , Translations
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


translation
    :: Tidings Translations
    -> Tidings Mode
    -> Behavior String
    -> UI ((Element, Element, Element), Tidings Popup.Mode, Tidings String)
translation tTranslations tMode bKey = mdo

    let bTranslations = facts tTranslations

    let bValue = (\k -> HashMap.lookupDefault k k) <$> bKey <*> bTranslations
    _text <- UI.span # sink text bValue

    let bOpen  = (\s -> "{{" ++ s ++ "}}") <$> bKey
    let bClose = HashMap.lookupDefault "close" "close" <$> bTranslations -- this wrong?
    ((_open, _close), tPopup) <- Popup.popup bOpen bClose

    let bToTranslate = HashMap.lookupDefault "" <$> bKey <*> bTranslations
    _translationInput <- Entry.entry bToTranslate
    _popup <- UI.div #+ [element _translationInput, element _close]

    let _translation = Entry.userText _translationInput

    return ((_text, _open, _popup), tPopup, _translation)




translation2
    :: Behavior Translations
    -> Behavior Mode
    -> Behavior String
    -> UI ((Element, Element, Element), Tidings Popup.Mode, Tidings String)
translation2 bTranslations bMode bKey = mdo

    ((open, close), tPopup) <- Popup.popup2 bOpen bClose

    text' <- UI.span # sink text bText
    input <- Entry.entry bTranslate
    popup <- UI.div #+ [element input, element close]

    let bText = (\k -> HashMap.lookupDefault k k) <$> bKey <*> bTranslations
    let bOpen  = (\s -> ("{{" ++ s ++ "}}")) <$> bKey
    let bClose = HashMap.lookupDefault "close" "close" <$> bTranslations
    let bTranslate = HashMap.lookupDefault "" <$> bKey <*> bTranslations

    let tTranslation = Entry.userText input

    return ((text', open, popup), tPopup, tTranslation)
