{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
module Lib.Client.Select.Dropdown
    ( dropdown
    , dropdown3
    , Mode(..)
    )
where


import qualified Lib.Client.Translation.Translation as Translation
import qualified Lib.Client.Select.Select      as Select
import qualified Lib.Model.Data                as Data
import           Prelude                 hiding ( get )
import qualified Relude.Unsafe                 as Unsafe
import           Utils.Comonad
import qualified Utils.ListZipper              as ListZipper
import qualified Relude.Unsafe                 as Unsafe
import           Control.Lens                   ( (^.)
                                                , (.~)
                                                , over
                                                , (%~)
                                                , lens
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


dropdown :: Behavior (a -> UI Element) -> Behavior (Bool -> a -> UI Element) 
    -> UI (Element, Behavior (ListZipper.ListZipper a -> [UI Element]), Behavior (ListZipper.ListZipper a -> [UI Element]), Tidings Mode, Event (Data.Data String (ListZipper.ListZipper a)))
dropdown bDisplayClosed bDisplayOpen = mdo

    opened   <- UI.div #. "buttons has-addons"

    (eSelection, hSelection) <- liftIO $ newEvent
    (ePopup , hPopup      ) <- liftIO $ newEvent

    let eSwitch = switch <$> Unsafe.head <$> unions
            [ bDropMode <@ eSelection
            , bDropMode <@ ePopup
            ]

    bDropMode <- stepper Closed $ eSwitch
    let tDropMode = tidings bDropMode eSwitch

    let bDisplayOpen' = bDisplayOpen <&> \f b (zipper :: ListZipper.ListZipper a) -> do
            display <- f b (extract zipper)

            UI.on UI.click display $ \_ -> do
                liftIO $ hSelection (Data.Data zipper)

            return $ display

    let bDisplayClosed' = bDisplayClosed <&> \f (zipper :: ListZipper.ListZipper a) -> do
            display <- f (extract zipper)

            UI.on UI.click display $ \_ -> do
                liftIO $ hPopup ()

            return $ display

    let bDisplayOpen'' = (\f z -> ListZipper.toList (ListZipper.bextend f z)) <$> bDisplayOpen'
    let bDisplayClosed'' = (\f z -> [f z]) <$> bDisplayClosed'

    return $ (opened, bDisplayClosed'', bDisplayOpen'', tDropMode, eSelection)


dropdown3 :: Behavior (a -> UI Element)
    -> Behavior (Bool -> a -> UI Element)
    -> UI (Element, Behavior (ListZipper.ListZipper a -> [UI Element]), Event (Data.Data String (ListZipper.ListZipper a)))
dropdown3 bDisplayClosed bDisplayOpen = mdo
    (opened, bViewClosed, bViewOpen,  tDropMode, eSelection) <- dropdown
        bDisplayClosed
        bDisplayOpen

    let bView = do
            viewClose <- bViewClosed
            viewOpen <- bViewOpen
            dropMode <- facts tDropMode
            return $ \zipper ->
                        case dropMode of
                                Open -> do
                                    [element opened # set children [] #+ (viewOpen zipper)]
                                Closed -> do
                                    viewClose zipper

    return (opened, bView, eSelection)
