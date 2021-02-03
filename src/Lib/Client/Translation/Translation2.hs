{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Translation.Translation2
    ( translation
    , Translations
    , toggle
    , mode
    )
where

import Lib.Client.Translation.Translation (Mode(..), Translations(..))
import Lib.Client.Input.Text
import qualified Lib.Client.Pop.Popup2 as Popup

import qualified Relude.Unsafe as Unsafe
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


toggle :: Mode -> Mode
toggle Normal = Searching
toggle Searching = Normal
toggle Translating = Normal

mode :: a -> a -> a -> Mode -> a
mode a _ _ Normal = a
mode _ b _ Searching = b
mode _ _ c Translating = c


translation :: Behavior Translations -> Behavior Mode -> Behavior String -> UI ((Element,Element,Element), Behavior Mode)
translation bTranslations bMode bKey = mdo

    let bValue = (\k -> HashMap.lookupDefault k k) <$> bKey <*> bTranslations
    let bToTranslate = HashMap.lookupDefault "" <$> bKey <*> bTranslations

    _text <- UI.span # sink text bValue

    let bOpen = (\s -> "{{"++s++"}}") <$> bKey
    let bClose = HashMap.lookupDefault "close" "close" <$> bTranslations
    ((_open, _close), tMode) <- Popup.popup bOpen bClose

    _translationInput <- entry bToTranslate
    _popup <- UI.div #+
        [ element _translationInput
        , element _close
        ]

    let _translation = userText _translationInput

    let bToggle = ((\toggle mode -> if toggle == Popup.Open then Translating else mode) <$> (facts tMode) <*> bMode) -- not facts pls

    return ((_text, _open, _popup), bToggle)
