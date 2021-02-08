{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Pop.Popup
    ( Mode(..)
    , popup
    , PopupEntry(..)
    )
where

import qualified Lib.Model.Data                as Data
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



data Mode
    = Closed
    | Open
    deriving (Eq, Show)

switch :: Mode -> Mode
switch Open = Closed
switch Closed = Open

data PopupEntry = PopupEntry
    { _close :: Element
    , _open :: Element
    , _tPopup :: Tidings Mode
    }

popup :: Behavior String -> Behavior String -> UI PopupEntry
popup bOpen bClose = mdo
    _open <- UI.button #. "button" # sink text bOpen
    _close <- UI.button #. "button" # sink text bClose

    let eOpen = bMode <@ UI.click _open
    let eClose = bMode <@ UI.click _close

    let eClick = Unsafe.head <$> unions
            [ eClose
            , eOpen
            ]

    bMode <- stepper Closed $ switch <$> eClick

    let _tPopup = tidings bMode eClick

    return PopupEntry { .. }
