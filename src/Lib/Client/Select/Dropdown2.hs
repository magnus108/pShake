{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Select.Dropdown2
    ( dropdown
    )
where


import qualified Lib.Client.Translation.Translation2 as Translation
import qualified Lib.Client.Translation.Translation as T
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

item' :: WriteAttr Element (UI Element)
item' = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ [i]

items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i


dropdown
    :: (Show a, Eq a)
    => Behavior T.Translations
    -> Behavior T.Mode
    -> Behavior (Data.Data String (ListZipper.ListZipper a))
    -> Behavior (Bool -> Bool -> a -> UI Element)
    -> UI ((Element, Element), Tidings Bool, Event (Data.Data String (ListZipper.ListZipper a)))
dropdown bTranslations bMode bZipper bDisplay = mdo

    _closed                     <- UI.div
    _open                       <- UI.div #. "buttons has-addons"

    -- bør måske sinke dem her endnu
    ((_notAskedView, _notAskedOpen, _notAskedClosed), bNotAsked) <- Translation.translation bTranslations bMode (pure "notAsked")
    ((_loadingView, _loadingOpen, _loadingClosed), bLoading)<- Translation.translation bTranslations bMode (pure "loading")
    ((_errorView, _errorOpen, _errorClosed), bError) <- Translation.translation bTranslations bMode (pure "error") -- (pure (\e-> show e))
    _loading <- UI.div # sink item (Translation.mode _loadingView _loadingOpen _loadingClosed <$> bLoading)
    _notAsked <- UI.div # sink item (Translation.mode _notAskedView _notAskedOpen _notAskedClosed <$> bNotAsked)
    _error <- UI.div # sink item (Translation.mode _errorView _errorOpen _errorClosed <$> bError)

    (_selection, _handleSelection) <- liftIO $ newEvent
    (_popup , _handlePopup      ) <- liftIO $ newEvent

    let e = Unsafe.head <$> unions (fmap not <$> [bState <@ _selection, bState <@ _popup])
    bState <- stepper False $ e


    let bDisplay' = bDisplay <&> \f h s b (zipper :: ListZipper.ListZipper a) -> do
            display <- f s b (extract zipper)

            UI.on UI.click display $ \_ -> do
                liftIO $ h (Data.Data zipper)

            return $ display

    element _closed # sink item' (Data.data' (return _notAsked) (return _loading) (\_ -> element _error) (\x -> x) <$>
            ((\display state zipper -> fmap (\item -> display _handlePopup state False item) zipper) <$> bDisplay' <*> bState <*> bZipper))
    element _open # sink items (Data.data' ([return _notAsked]) ([return _loading]) (\_ -> [element _error]) (\x -> x) <$> ((\display state zipper -> fmap (\item -> ListZipper.toList (ListZipper.bextend (display _handleSelection state) item) ) zipper) <$> bDisplay' <*> bState <*> bZipper))

    return $ ((_closed, _open), tidings bState e, _selection)
