{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Select.Dropdown
    ( Dropdown(..)
    , dropdown
    )
where

import qualified Lib.Client.Select.Select               as Select
import qualified Lib.Model.Data                as Data
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


data Dropdown a = Dropdown
    { _container :: Element
    , _selection :: Event (Data.Data String (ListZipper.ListZipper a))
    }

instance Widget (Dropdown a) where
    getElement = _container


data Mode
    = Closed
    | Open

test :: UI (Dropdown String)
test = mdo

    let display = pure (\x -> show x)

    dropdown' <- dropdown content display

    let eStateChange = _selection dropdown'

    content <- stepper (Data.Data (ListZipper.ListZipper [] "a" ["a","b"])) eStateChange

    return dropdown'



dropdown :: (Show a, Eq a) => Behavior (Data.Data String (ListZipper.ListZipper a)) -> Behavior (a -> String) -> UI (Dropdown a)
dropdown bZipper bDisplay = do

    _container <- UI.div

    _open <- UI.button #. "button"
    _close <- UI.button #. "button" # set text "close"

    state <- stepper Closed $ Unsafe.head <$>
        unions [ Closed <$ UI.click _close
               , Open <$ UI.click _open
               ]


-----------------
    let bDisplay2 = bDisplay <&> \f b x -> let button = UI.button #. "button" # set text (f x) in
                                        if b then button #. "button is-info is-selected" else button

    selectors <- Select.select bZipper bDisplay2

    let eSelection = Select._selection selectors
    eh2 <- element selectors
    eh <- UI.div # set children [ eh2, _close]
-----------------

    -- is this dangerous?
    let bDisplay' = bDisplay <&> \f state' (zipper :: ListZipper.ListZipper a) -> do
                                    case state' of
                                            Open -> do
                                                element eh
                                            Closed -> do
                                                let focus = extract zipper
                                                let display = f focus
                                                element _open # set text display

    let view = (\d s z -> fmap (d s) z) <$> bDisplay' <*> state <*> bZipper

    _ <- element _container # sink items view

    let _selection = eSelection

    return Dropdown { .. }


items :: WriteAttr Element (Data.Data String (UI Element))
items = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            return x # set children [] #+ [item]
