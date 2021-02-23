{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ApplicativeDo #-}
module Lib.Client.Translation.Translation
    ( Mode(..)
    , toggle
    , translation
    , translation2
    , TranslationEntry(..)
    )
where

import qualified Control.Lens                  as Lens

import qualified Lib.Model.Translation         as Translation
import qualified Lib.Client.Input.Text         as Entry
import qualified Lib.Client.Pop.Popup          as Popup

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


data TranslationEntry = TranslationEntry
    { _text :: Element
    , _close :: Element
    , _open :: Element
    , _input :: Element
    , _eInput :: Event String
    , _tPopup :: Tidings Popup.Mode
    }

translation
    :: Behavior Translation.Translations
    -> Behavior String
    -> UI TranslationEntry
translation bTranslations' bKey = mdo

    let bTranslations = Lens.view Translation.unTranslation <$> bTranslations'
    popup <- Popup.popup bOpen bClose
    let _close  = Popup._close popup
    let _open   = Popup._open popup
    let _tPopup = Popup._tPopup popup

    _text <- UI.span # sink text bText
    input <- Entry.entry bTranslate

    let bText = (\k -> HashMap.lookupDefault k k) <$> bKey <*> bTranslations
    let bOpen      = (\s -> ("{{" ++ s ++ "}}")) <$> bKey
    let bClose     = HashMap.lookupDefault "close" "close" <$> bTranslations
    let bTranslate = HashMap.lookupDefault "" <$> bKey <*> bTranslations

    let _eInput    = rumors (Entry.userText input)
    _input <- element input

    return TranslationEntry { .. }


translation2
    :: Behavior Translation.Translations
    -> Behavior Mode
    -> Behavior String
    -> UI (Behavior [UI Element], (Event String, Behavior String))
translation2 bTranslations bTransMode bKey = mdo

    trans <- translation bTranslations bKey

    let bView = do
            transMode <- bTransMode
            popupMode <- facts (_tPopup trans)
            return $ fmap element $ case (transMode, popupMode) of
                (Translating, Popup.Open) -> do
                    [_input trans, _close trans]
                (Translating, Popup.Closed) -> [_open trans]
                _                           -> [_text trans]

    let keyValue = (_eInput trans, bKey)

    return (bView, keyValue)
