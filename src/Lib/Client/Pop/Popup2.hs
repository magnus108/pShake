{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Pop.Popup2
    ( popup
    , Mode(..)
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

popup :: Behavior String -> Behavior String -> UI ((Element,Element), Tidings Mode)
popup open close = mdo
    _buttonOpen <- UI.button #. "button" # sink text open
    _buttonClose <- UI.button #. "button" # sink text close

    let _eOpen = bMode <@ UI.click _buttonOpen
    let _eClose = bMode <@ UI.click _buttonClose

    let _e = Unsafe.head <$> unions
            [ _eClose
            , _eOpen
            ]

    bMode <- stepper Closed $ switch <$> _e

    return ((_buttonClose, _buttonOpen), tidings bMode _e)
