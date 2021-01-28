{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Picker.Picker
    ( picker
    , Picker(..)
    )
where

import qualified Lib.Model.Data                as Data
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


data Picker = Picker
    { _container :: Element
    , _selection :: Event ()
    }

instance Widget Picker where
    getElement = _container


picker :: Behavior (Data.Data String FilePath) -> Behavior (FilePath -> UI Element) -> Behavior (UI Element) -> UI Picker
picker bItem bDisplay bFallBack = mdo
    _container <- UI.div

    _selector  <- UI.button

    let bDisplay' = bDisplay <&> \f filepath  -> do
            icon <- UI.span #. "icon" #+ [UI.mkElement "i" #. "far fa-file"]
            text <- f filepath
            element _selector #. "button"
                # set children [] #+ fmap element [text, icon]

    let bFallback' = bFallBack <&> \text s -> do
            element _selector #. "button"
                # set children [] #+ [text]

    _ <- element _container # sink items (bimap <$> bFallback' <*> bDisplay' <*> bItem)

    let _selection = UI.click _selector

    return Picker {..}


items :: WriteAttr Element (Data.Data (UI Element) (UI Element))
items = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            return x # set children [] #+ [e]
        Data.Data item -> do
            return x # set children [] #+ [item]
