{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Select.Select
    ( Select(..)
    , select
    )
where

import Control.Conditional ((?<>))
import qualified Lib.Client.Translation.Translation as ClientTranslation
import qualified Lib.Model.Translation                as Translation

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
    -> Behavior ClientTranslation.Mode
    -> Behavior (Data.Data String (ListZipper.ListZipper a))
    -> Behavior (a -> UI Element)
    -> Behavior (String -> UI Element)
    -> UI (Select a)
select bTranslations bMode bZipper bDisplay bFallback = mdo
    (_selection, _handle) <- liftIO $ newEvent
    _container             <- UI.div #. "buttons has-addons"

    -- is this dangerous?
    let bDisplay' =
            (\f m z -> do
                    c <- sequence $ ListZipper.toList $ ListZipper.bextend (f m) z
                    element _container # set children c
                )
                <$> (\f m b (zipper :: ListZipper.ListZipper a) -> do
                        trans <- f (extract zipper)
                        display <- UI.button #. (b ?<> "is-info is-seleceted" <> " " <> "button") # set children [trans]

                        UI.on UI.click display $ \_ -> do
                            liftIO $ _handle (Data.Data zipper)

                        return $ if m /= ClientTranslation.Normal then trans else display
                    )
                <$> bDisplay <*> bMode

    notAsked <- UI.div --Translation.translation bTranslations bMode (pure "notAsked")
    loading  <- UI.div -- Translation.translation bTranslations bMode (pure "loading")

    _        <- element _container # sink
        items
        (   Data.data' (return notAsked) (return loading)
            --(element notAsked) (element loading)
        <$> bFallback
        <*> bDisplay'
        <*> bZipper
        )

    return Select { .. }


items :: WriteAttr Element (UI Element)
items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ [i]
