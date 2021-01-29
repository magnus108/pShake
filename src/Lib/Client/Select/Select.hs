{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Select.Select
    ( Select(..)
    , select
    )
where

import Control.Conditional ((?<>))
import qualified Lib.Client.Translation.Translation
                                               as Translation
import qualified Data.HashMap.Strict           as HashMap
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


select
    :: (Show a, Eq a)
    => Behavior Translation.Translations
    -> Behavior Translation.Mode
    -> Behavior (Data.Data String (ListZipper.ListZipper a))
    -> Behavior (Bool -> a -> UI Element)
    -> Behavior (String -> UI Element)
    -> UI (Select a)
select bTranslations bMode bZipper bDisplay bFallback = mdo
    (_selection, _handle) <- liftIO $ newEvent

    _container            <- UI.div
    _children             <- UI.div

    -- is this dangerous?
    let bDisplay' =
            (\f m z -> do
                    c <- sequence $ ListZipper.toList $ ListZipper.bextend (f m) z
                    element _children # set children c
                )
                <$> (\f m b (zipper :: ListZipper.ListZipper a) -> do
                        trans <- f b (extract zipper)
                        display <- UI.button #. (b ?<> "is-info is-seleceted" <> " " <> "button") #+ [element trans]

                        UI.on UI.click display $ \_ -> do
                            liftIO $ _handle (Data.Data zipper)

                        return $  if m /= Translation.Normal then trans else display
                    )
                <$> bDisplay <*> bMode

    notAsked <- Translation.translation bTranslations bMode (pure "notAsked")
    loading  <- Translation.translation bTranslations bMode (pure "loading")

    _        <- element _container # sink
        items
        (   Data.data' (element notAsked) (element loading)
        <$> bFallback
        <*> bDisplay'
        <*> bZipper
        )

    return Select { .. }


items :: WriteAttr Element (UI Element)
items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ [i]
