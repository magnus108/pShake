{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Lib.Client.Select.Dropdown2
    ( dropdown2
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


dropdown2 :: Eq a => Behavior Translation.Translations 
    -> Behavior Translation.Mode -> Behavior (Data.Data String (ListZipper.ListZipper a))
    -> Behavior (a -> UI Element)
    -> Behavior (Bool -> a -> UI Element)
    -> UI (Behavior (Data.Data String ([UI Element])), Tidings Mode, Event (Data.Data String (ListZipper.ListZipper a)))
dropdown2 bTranslations bTransMode bZipper bDisplayClosed bDisplayOpen = mdo

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

    let bItem = (\opened closed mode zipper ->
                    if mode == Open then
                        fmap (\z -> ListZipper.toList (ListZipper.bextend opened z)) zipper
                    else
                        fmap (\z -> [closed z]) zipper
                ) <$> bDisplayOpen' <*> bDisplayClosed' <*> bDropMode <*> bZipper

    return $ (bItem, tDropMode, eSelection)
