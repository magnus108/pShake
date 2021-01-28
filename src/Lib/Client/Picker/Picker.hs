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

    _selector  <- UI.button #. "button"

    _ <- element _container # sink (items _selector) ((\x y -> (x,y)) <$> (fmap <$> bDisplay <*> bItem) <*> bFallBack)

    let _selection = UI.click _selector

    return Picker {..}


items :: Element -> WriteAttr Element ((Data.Data String (UI Element)), UI Element)
items picker = mkWriteAttr $ \(i, f) x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            element picker # set children [] #+ [f]
            return x # set children [] #+ [element err, element picker]
        Data.Data item -> do
            element picker # set children [] #+ [item]
            return x # set children [] #+ [element picker]
