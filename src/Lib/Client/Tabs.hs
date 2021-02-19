{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ApplicativeDo #-}
module Lib.Client.Tabs
    ( tabs
    , Tabs(..)
    )
where

import           Control.Conditional            ( (?<>) )

import qualified Control.Lens                  as Lens

import qualified Lib.Client.Translation.Translation
                                               as Translation
import qualified Lib.Client.Select.Select      as Select
import qualified Lib.Model.Data                as Data
import qualified Lib.Model.Tab                as Tab
import           Prelude                 hiding ( get )
import qualified Relude.Unsafe                 as Unsafe
import           Utils.Comonad
import qualified Utils.ListZipper              as ListZipper
import qualified Lib.Model.Tab                 as Tab
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



data Tabs = Tabs
    { _container :: Element
    , _selection :: Event Tab.Tabs
    --, _eTransNotAsked :: (Event String, Behavior String)
    --, _eTransLoading :: (Event String, Behavior String)
    --, _eTransError :: (Event String, Behavior String)
    }

instance Widget Tabs where
    getElement = _container

lookup                  :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup _key []          =  Nothing
lookup  key ((x,y):xys)
    | key == x           =  Just y
    | otherwise         =  lookup key xys

tabs :: Behavior Translation.Translations
    -> Behavior Translation.Mode
    -> [(Tab.Tab, (Behavior [UI Element], (Event String, Behavior String)))]
    -> Behavior (Data.Data String Tab.Tabs)
    -> UI Tabs
tabs bTranslations bTransMode translations bTabs = do
    
    let translations' = translations <&> (\(k,(v,_)) -> (\v' -> (k,v')) <$> v)
    -- (key, UI children)
    let translations'' = sequenceA translations'

    let bZipper = Lens.view Tab.unTabs <<$>> bTabs

    let ggMAX = (\zip trans ->  fmap (\z -> fmap (\t -> (t, Unsafe.fromJust (lookup t trans ))) z ) zip) <$> bZipper <*> translations''


    (eSelection, hSelection) <- liftIO $ newEvent

    let bDisplay = pure $ \center (tabs :: ListZipper.ListZipper (Tab.Tab, [UI Element])) -> do
                display <- UI.button
                    #. (center ?<> "is-info is-seleceted" <> " " <> "button")
                    #+ (snd (extract tabs))

                UI.on UI.click display $ \_ -> do
                    liftIO $ hSelection (Data.Data (fmap fst tabs))

                return $ display

    let bDisplay'' = (\f mode z -> case mode of
                            Translation.Translating ->
                                [UI.div #+ (concat (fmap snd (ListZipper.toList z)))]
                            Translation.Normal -> do
                                [UI.div #. "buttons has-addons" #+ ListZipper.toList (ListZipper.bextend f z)]
                     ) <$> bDisplay <*> bTransMode


    (errorView, _eTransError) <- Translation.translation2 bTranslations
                                                          bTransMode
                                                          (pure "error")
    (loadingView, _eTransLoading) <- Translation.translation2
        bTranslations
        bTransMode
        (pure "loading")

    (notAskedView, _eTransNotAsked) <- Translation.translation2
        bTranslations
        bTransMode
        (pure "notAsked")

--"buttons has-addons"
    _container   <- UI.div # sink items (Data.data'' <$> loadingView <*> notAskedView <*> errorView <*> bDisplay'' <*> ggMAX)

    let _selection = fmap Tab.Tabs $ filterJust $ Data.toJust <$> eSelection

    return Tabs { .. }


items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i
