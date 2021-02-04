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


item :: WriteAttr Element Element
item = mkWriteAttr $ \i x -> void $ do
    return x # set children [i]


items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i


dropdown
    :: (Show a, Eq a)
    => Tidings Translation.Translations
    -> Tidings Translation.Mode
    -> Behavior (Data.Data String (ListZipper.ListZipper a))
    -> Behavior (Bool -> Bool -> a -> UI Element)
    -> UI ((Element, Element), Tidings Bool, Event (Data.Data String (ListZipper.ListZipper a)))
dropdown tTranslations tMode bZipper bDisplay = mdo

    (_, tNotAsked) <- Translation.translation tTranslations tMode (pure "notAsked")
    (_, tLoading) <- Translation.translation tTranslations tMode (pure "loading")
    (_, tError) <- Translation.translation tTranslations tMode (pure "error")

    _loading <- UI.div # sink item (facts tLoading)
    _notAsked <- UI.div # sink item (facts tNotAsked)
    _error <- UI.div # sink item (facts tError)

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


    _closed                     <- UI.div
    let ggg = flip fmap <$> bZipper <*> ((\display state -> display _handlePopup state False) <$> bDisplay' <*> bState)
    window <- askWindow
    liftIOLater $ Reactive.onChange ggg $ \s -> runUI window $ do
        for_ s $ \x -> element _closed # set children [] #+ [x]


    let ggg2 = flip fmap <$> bZipper <*> ((\display state zipper -> ListZipper.toList (ListZipper.bextend (display _handleSelection state) zipper)) <$> bDisplay' <*> bState)
    _open                       <- UI.div #. "buttons has-addons"
    liftIOLater $ Reactive.onChange ggg2 $ \s -> runUI window $ do
        for_ s $ \x -> element _open # set children [] #+ x

    return $ ((_closed, _open), tState, _selection)
