{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Select.Dropdown2
    ( dropdown
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


dropdown
    :: (Show a, Eq a)
    => Tidings Translation.Translations
    -> Tidings Translation.Mode
    -> Behavior (Data.Data String (ListZipper.ListZipper a))
    -> Behavior (Bool -> Bool -> a -> UI Element)
    -> UI ((Element, Element), Tidings Bool, Event (Data.Data String (ListZipper.ListZipper a)))
dropdown tTranslations tMode bZipper bDisplay = mdo

    (_selection, _handleSelection) <- liftIO $ newEvent
    (_popup , _handlePopup      ) <- liftIO $ newEvent

    let e = Unsafe.head <$> unions (fmap not <$> [bState <@ _selection, bState <@ _popup])
    bState <- stepper False $ e
    let tState = tidings bState e

    let bDisplay' = bDisplay <&> \f h s b (zipper :: ListZipper.ListZipper a) -> do
            display <- f s b (extract zipper)

            UI.on UI.click display $ \_ -> do
                liftIO $ h (Data.Data zipper)

            return $ display

    _closed <- open _handlePopup bState bDisplay' bZipper
    _open <- open _handleSelection bState bDisplay' bZipper

    return $ ((_closed, _open), tState, _selection)



open _handleSelection bState bDisplay bZipper = do
    _open                       <- UI.div #. "buttons has-addons"

    let bItem = (\x y z -> (x,y,z)) <$> bDisplay <*> bState <*> bZipper

    window <- askWindow
    liftIOLater $ Reactive.onChange bItem $ \(d,s,z) -> runUI window $ do
        for_ z $ \z' -> do
            let children' = ListZipper.toList (ListZipper.bextend (d _handleSelection s) z')
            element _open # set children [] #+ children'

    return _open


closed _handlePopup bState bDisplay bZipper = do
    _closed                       <- UI.div

    let bItem = (\x y z -> (x,y,z)) <$> bDisplay <*> bState <*> bZipper

    window <- askWindow
    liftIOLater $ Reactive.onChange bItem $ \(d,s,z) -> runUI window $ do
        for_ z $ \z' -> do
            let child = d _handlePopup s False z'
            element _closed # set children [] #+ [child]

    return _closed
