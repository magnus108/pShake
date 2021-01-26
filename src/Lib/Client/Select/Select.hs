{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Select.Select
    ( Select(..)
    , select
    )
where

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


data Select a = Select
    { _container :: Element
    , _selection :: Event (Data.Data String (ListZipper.ListZipper a))
    }

instance Widget (Select a) where
    getElement = _container


select :: (Show a, Eq a) => Behavior (Data.Data String (ListZipper.ListZipper a)) -> Behavior (Bool -> a -> UI Element) -> UI (Select a)
select bZipper bDisplay = do
    (_selection, _handle) <- liftIO $ newEvent

    _container <- UI.div

    -- is this dangerous?
    let bDisplay' = bDisplay <&> \f b (zipper :: ListZipper.ListZipper a) -> do
                                    display <- f b (extract zipper)

                                    UI.on UI.click display $ \_ -> do
                                        liftIO $ _handle (Data.Data zipper)

                                    return $ display

    let bButtons = (\d z -> fmap (ListZipper.bextend d) z) <$> bDisplay' <*> bZipper

    _ <- element _container # sink items bButtons

    return Select { .. }


items :: WriteAttr Element (Data.Data String (ListZipper.ListZipper (UI Element)))
items = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            return x # set children [] #+ (ListZipper.toList item)
