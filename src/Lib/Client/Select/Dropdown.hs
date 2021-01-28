{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Select.Dropdown
    ( Dropdown(..)
    , dropdown
    )
where
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


data Dropdown a = Dropdown
    { _container :: Element
    , _selection :: Event (Data.Data String (ListZipper.ListZipper a))
    }

instance Widget (Dropdown a) where
    getElement = _container


dropdown
    :: (Show a, Eq a)
    => Behavior (Data.Data String (ListZipper.ListZipper a))
    -> Behavior (Bool -> Bool -> a -> UI Element)
    -> UI (Dropdown a)
dropdown bZipper bDisplay = mdo

    (_selection, _handleSelection) <- liftIO $ newEvent
    (_pop      , _handlePop      ) <- liftIO $ newEvent

    _container                     <- UI.div #. "buttons has-addons"

    bState                         <- stepper False $ Unsafe.head <$> unions
        [fmap not (bState <@ _selection), fmap not (bState <@ _pop)]

    let bDisplay' = bDisplay <&> \f h s b (zipper :: ListZipper.ListZipper a) -> do
            display <- f s b (extract zipper)

            UI.on UI.click display $ \_ -> do
                liftIO $ h (Data.Data zipper)

            return $ display

    let view =
            (\display'' state' zipper' -> fmap
                    (\item -> if state'
                        then ListZipper.toList
                            (ListZipper.bextend (display'' _handleSelection state') item)
                        else [display'' _handlePop state' False item]
                    )
                    zipper'
                )
                <$> bDisplay'
                <*> bState
                <*> bZipper


    _ <- element _container # sink items view

    return Dropdown { .. }


items :: WriteAttr Element (Data.Data String [UI Element])
items = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            return x # set children [] #+ item

