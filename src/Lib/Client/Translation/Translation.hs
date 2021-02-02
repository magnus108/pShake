{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Translation.Translation
    ( Mode(..)
    , Translation(..)
    , translation
    , Translations
    , toggle

    , translation2
    )
where

import Lib.Client.Input.Text

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



data Mode
    = Translating
    | Searching
    | Normal
    deriving (Eq, Show)


type Translations = HashMap String String


toggle :: Mode -> Mode
toggle Normal = Searching
toggle Searching = Normal
toggle Translating = Normal


data Translation = Translation
    { _container :: Element
    , _buttonOpen :: Element
    , _text :: Element
    , _translation :: Tidings String
    }

instance Widget Translation where
    getElement = _container



    {-
type DataItem = (String, String)
showDataItem (firstname, lastname) = lastname ++ ", " ++ firstname

-- | Data item widget, consisting of two text entries
dataItem
    :: Behavior (Maybe DataItem)
    -> UI ((Element, Element), Tidings DataItem)
dataItem bItem = do
    entry1 <- UI.entry $ fst . maybe ("","") id <$> bItem
    entry2 <- UI.entry $ snd . maybe ("","") id <$> bItem
    
    return ( (getElement entry1, getElement entry2)
           , (,) <$> UI.userText entry1 <*> UI.userText entry2
           )
translation3 :: UI (Behavior (Either Element Element))
translation3 = mdo
    _buttonOpen <- UI.button #. "button" # set text "open3"
    _buttonClose <- UI.button #. "button" # set text "close3"

    let _eOpen = bMode <@ UI.click _buttonOpen
    let _eClose = bMode <@ UI.click _buttonClose

    let _e = Unsafe.head <$> unions
            [ _eClose
            , _eOpen
            ]

    bMode <- stepper Closed $ switch <$> _e

    return $ (\m -> case m of
            Closed -> Left _buttonClose
            Open -> Right _buttonOpen ) <$> bMode
            -}

translation2 :: Translations -> Mode -> Bool -> String -> UI Element
translation2 translations mode toggle key = mdo
    let value = HashMap.lookupDefault key key translations

    _text <- UI.span # set text value
    _buttonOpen <- UI.button #. "button" # set text ( (\s -> "{{"++s++"}}") key)
    _buttonClose <- UI.button #. "button" # set text "close"

    let toTranslate = HashMap.lookupDefault "" key translations
    _translationInput <- entry (pure toTranslate)
    let _translation = userText _translationInput
    _popup <- UI.div #+
        [ element _translationInput
        , element _buttonClose
        ]

    let eOpenPopup = UI.click _buttonOpen
    let eClosePopup = UI.click _buttonClose

    if toggle then
        return _popup
    else
        case mode of
            Searching -> return _popup
            Normal -> return _text
            Translating -> return _popup



translation :: Behavior Translations -> Behavior Mode -> Behavior String -> UI Translation
translation bTranslations bMode bKey = mdo

    _container <- UI.div

    let bValue = (\k -> HashMap.lookupDefault k k) <$> bKey <*> bTranslations
    _text <- UI.span # sink text bValue

    _buttonOpen <- UI.button #. "button" # sink text ( (\s -> "{{"++s++"}}") <$> bKey)
    _buttonClose <- UI.button #. "button" # set text "close"


    let bToTranslate = HashMap.lookupDefault "" <$> bKey <*> bTranslations
    _translationInput <- entry bToTranslate



    let _translation = userText _translationInput
    _popup <- UI.div #+
        [ element _translationInput
        , element _buttonClose
        ]

    let eOpenPopup = UI.click _buttonOpen
    let eClosePopup = UI.click _buttonClose

    bToggle <- stepper False $ Unsafe.head <$> unions
        [ (not <$> bToggle) <@ eOpenPopup
        , (not <$> bToggle) <@ eClosePopup
        ]

    element _container # sink (translate _text _buttonOpen _popup) ((\toggle mode -> if toggle then Translating else mode) <$> bToggle <*> bMode)

    return Translation { .. }


translate :: Element -> Element -> Element -> WriteAttr Element Mode
translate text button popup = mkWriteAttr $ \i x -> void $ do
    case i of
        Searching -> return x # set children [button]
        Normal -> return x # set children [text]
        Translating -> return x # set children [popup]
