{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ApplicativeDo #-}
module Lib.Client.Translation.Translation
    ( Translations
    , Mode(..)
    , toggle
    , translation
    , translation2
    , TranslationEntry(..)
    )
where

import qualified Lib.Model.Data                as Data
import qualified Lib.Client.Input.Text         as Entry
import qualified Lib.Client.Pop.Popup         as Popup

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

data TranslationEntry = TranslationEntry
    { _text :: Element
    , _close :: Element
    , _open :: Element
    , _input :: Element
    , _eInput :: Event String
    , _tPopup :: Tidings Popup.Mode
    }

translation
    :: Behavior Translations
    -> Behavior String
    -> UI TranslationEntry
translation bTranslations bKey = mdo

    popup <- Popup.popup bOpen bClose
    let _close = Popup._close popup
    let _open = Popup._open popup
    let _tPopup = Popup._tPopup popup

    _text                      <- UI.span # sink text bText
    input                      <- Entry.entry bTranslate

    let bText = (\k -> HashMap.lookupDefault k k) <$> bKey <*> bTranslations
    let bOpen      = (\s -> ("{{" ++ s ++ "}}")) <$> bKey
    let bClose     = HashMap.lookupDefault "close" "close" <$> bTranslations
    let bTranslate = HashMap.lookupDefault "" <$> bKey <*> bTranslations

    let _eInput    = rumors (Entry.userText input)
    _input <- element input

    return TranslationEntry { .. }


translation2
    :: Behavior Translations
    -> Behavior Mode
    -> Behavior String
    -> UI (Behavior (Element -> UI Element), (Event String, Behavior String))
translation2 bTranslations bTransMode bKey = mdo
    trans <- translation bTranslations bKey

    let bView = do
            transMode <- bTransMode
            popupMode <- facts (_tPopup trans)
            return $ \x -> case (transMode, popupMode) of
                    (Translating, Popup.Open) -> do
                        return x # set children [_input trans, _close trans]
                    (Translating, Popup.Closed) ->
                        return x # set children [_open trans]
                    _ -> return x # set children [_text trans]

    let keyValue = (_eInput trans, bKey)

    return (bView, keyValue)
