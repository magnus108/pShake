{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Pop.Popup
    ( Popup(..)
    , popup
    , Mode(..)
    , popup3
    , popup4
    )
where

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


import qualified Lib.Client.Translation.Translation as Translation

data Mode
    = Closed
    | Open
    deriving (Eq, Show)

switch :: Mode -> Mode
switch Open = Closed
switch Closed = Open


data Popup = Popup
    { _container :: Behavior (UI Element)
    , _tPop :: Tidings Mode
    }

popup :: Behavior (Element -> UI Element) -> Behavior (Element -> UI Element) -> UI Popup
popup bf bg = mdo
    _buttonOpen <- UI.button #. "button" # set text "open"
    _buttonClose <- UI.button #. "button" # set text "close"

    let _eOpen = _bMode <@ UI.click _buttonOpen
    let _eClose = _bMode <@ UI.click _buttonClose

    let _e = Unsafe.head <$> unions
            [ _eClose
            , _eOpen
            ]

    _bMode <- stepper Closed $ (\x -> case x of 
                                            Open -> Closed
                                            Closed -> Open
                                    ) <$> _e

    let _container = (\mode f g -> case mode of
                        Closed -> f _buttonClose
                        Open -> g _buttonOpen
                        ) <$> _bMode <*> bf <*> bg


    let _tPop = tidings _bMode _e

    return Popup { .. }


popup4 :: UI (Behavior (Either Element Element))
popup4 = mdo
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


popup3 :: Behavior (Element -> UI Element) -> Behavior (Element -> UI Element) -> UI (Behavior (UI Element))
popup3 bf bg = mdo
    _buttonOpen <- UI.button #. "button" # set text "open3"
    _buttonClose <- UI.button #. "button" # set text "close3"

    let _eOpen = bMode <@ UI.click _buttonOpen
    let _eClose = bMode <@ UI.click _buttonClose

    let _e = Unsafe.head <$> unions
            [ _eClose
            , _eOpen
            ]

    bMode <- stepper Closed $ switch <$> _e

    return $ (\m f g -> case m of
            Closed -> f _buttonClose
            Open -> g _buttonOpen ) <$> bMode <*> bf <*> bg
