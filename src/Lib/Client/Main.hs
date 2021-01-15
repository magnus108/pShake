{-# LANGUAGE RecursiveDo #-}
module Lib.Client.Main
    ( dumpCount
    )
where

import qualified Reactive.Threepenny           as Reactive
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny        as UI

import qualified Lib.Model.DumpDir                as DumpDir
import qualified Lib.Model.Data                as Data

data Count = Count
    { _elementCount :: Element
    }

instance Widget Count where
    getElement = _elementCount

dumpCount :: Behavior (Data.Data String DumpDir.DumpDir) -> UI Count
dumpCount bItems = do

    _elementCount <- UI.div

    element _elementCount # sink count bItems

    return Count { .. }

count = mkWriteAttr $ \i x -> void $ do
    case i of
        Data.NotAsked  -> return x # set text "Not Asked"
        Data.Loading   -> return x # set text "bobo"
        Data.Failure e -> do
            err <- string (show e)
            return x # set children [] #+ [element err]
        Data.Data item -> do
            return x # set text (show (length (DumpDir.unDumpDir item)))

