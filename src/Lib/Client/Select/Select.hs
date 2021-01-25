{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Select.Select
    ( Select(..)
    , select
    )
where

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
    , _selections :: [Event (ListZipper.ListZipper a)]
    }

instance Widget (Select a) where
    getElement = _container


ggg :: UI (Select String)
ggg = mdo
    let bDisplay = pure $ \x -> UI.button # set text x

    selectors <- select bZipper bDisplay

    let eSelections = _selections selectors

    bZipper <- stepper (ListZipper.ListZipper [] "h" ["h2"]) $ Unsafe.head <$> unions eSelections

    return selectors



select :: Behavior (ListZipper.ListZipper a) -> Behavior (a -> UI Element) -> UI (Select a)
select bZipper bDisplay = do

    _container <- UI.div

    {-
    let bDisplay' = bDisplay <&> \f (zipper :: ListZipper.ListZipper a) -> do
                                    display <- f (extract zipper)
                                    let event = zipper <$ UI.click display
                                    return $ (display, event)
                                    -}

    let bDisplay' = bDisplay <&> \f (zipper :: ListZipper.ListZipper a) -> (f (extract zipper), zipper)
    --bButtons :: Behavior (ListZipper.ListZipper (UI Element, Event (ListZipper.ListZipper a))))
    let bButtons = extend <$> bDisplay' <*> bZipper

    element _container # sink items (fmap fst <$> bButtons)

    let _selections = [UI.never]

    return Select { .. }

sink2 :: Behavior (ListZipper.ListZipper (UI Element, ListZipper.ListZipper a)) -> UI x -> UI (x, [Event (ListZipper.ListZipper a)])
sink2 bi mx = do
    x <- mx

    window <- askWindow
    i <- currentValue bi

    events <- sequence (fmap (\i' -> do
                    display <- fst i'
                    let e = snd i' <$ UI.click display
                    return e
                 ) i)
    let events2 = toList events
    _
    {-
    liftIOLater $ runUI window $ void $ do
        return x # set children [] #+ (ListZipper.toList (fmap fst i))
        -}

    {-
    liftIOLater $ do
        i <- currentValue bi
        Reactive.onChange bi  $ \i -> runUI window $ do
            return x # set children [] #+ (ListZipper.toList (fmap fst i))

-}

    return (x, [])

items :: WriteAttr Element (ListZipper.ListZipper (UI Element))
items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ (ListZipper.toList i)
