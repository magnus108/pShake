{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Select.Select
    ( Select(..)
    , select
    , test
    )
where

import Prelude hiding (get)
import qualified Relude.Unsafe as Unsafe
import Utils.Comonad
import qualified Utils.ListZipper              as ListZipper
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


data Select a = Select
    { _container :: Element
    , _selection :: Event (ListZipper.ListZipper a)
    }

instance Widget (Select a) where
    getElement = _container


test :: UI (Select String)
test = mdo
    let bDisplay = pure $ \b x -> let button = UI.button # set text x in
                                        if b then set style [("color", "blue")] button else button

    selectors <- select bZipper bDisplay

    let eSelection = _selection selectors

    bZipper <- stepper (ListZipper.ListZipper [] "h" ["h2"]) eSelection

    return selectors



select :: (Show a, Eq a) => Behavior (ListZipper.ListZipper a) -> Behavior (Bool -> a -> UI Element) -> UI (Select a)
select bZipper bDisplay = do
    (_selection, _handle) <- liftIO $ newEvent

    _container <- UI.div

    -- is this dangerous?
    let bDisplay' = bDisplay <&> \f b (zipper :: ListZipper.ListZipper a) -> do
                                    display <- f b (extract zipper)

                                    UI.on UI.click display $ \_ -> do
                                        liftIO $ _handle zipper

                                    return $ display

    let bButtons = ListZipper.bextend <$> bDisplay' <*> bZipper

    _ <- element _container # sink items bButtons

    return Select { .. }


items :: WriteAttr Element (ListZipper.ListZipper (UI Element))
items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ (ListZipper.toList i)
