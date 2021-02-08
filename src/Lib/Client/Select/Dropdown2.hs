{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module Lib.Client.Select.Dropdown2
    ( dropdown2
    , Mode(..)
    )
where


import qualified Lib.Client.Translation.Translation2 as Translation
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


writeOpen handle = mkWriteAttr $ \(d,s,z) x -> void $ do
                        for_ z $ \z' -> do
                                    let children' = ListZipper.toList (ListZipper.bextend (d handle s) z')
                                    return x # set children [] #+ children'

writeClose handle = mkWriteAttr $ \(d,s,z) x -> void $ do
                        for_ z $ \z' -> do
                                let child = d handle s False z'
                                return x # set children [] #+ [child]

    -- -> UI ((Element, Element), Tidings Mode, Event (Data.Data String (ListZipper.ListZipper a)))
dropdown2 bTranslations bTransMode bZipper bDisplay = mdo

    (eSelection, hSelection) <- liftIO $ newEvent
    (ePopup , hPopup      ) <- liftIO $ newEvent

    let eSwitch = switch <$> Unsafe.head <$> unions
            [ bDropMode <@ eSelection
            , bDropMode <@ ePopup
            ]

    bDropMode <- stepper Closed $ eSwitch
    let tDropMode = tidings bDropMode eSwitch

    let bDisplay' = bDisplay <&> \f h m b (zipper :: ListZipper.ListZipper a) -> do
            display <- f m b (extract zipper)

            UI.on UI.click display $ \_ -> do
                liftIO $ h (Data.Data zipper)

            return $ display

    let bItem = (\d s z -> fmap (\z' -> (d,s,z')) z) <$> bDisplay' <*> bDropMode <*> bZipper

    return $ (bItem, hSelection, hPopup, eSelection)
