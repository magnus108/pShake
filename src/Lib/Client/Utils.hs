module Lib.Client.Utils
    ( items
    )
where

import           Graphics.UI.Threepenny.Core

items :: WriteAttr Element [UI Element]
items = mkWriteAttr $ \i x -> void $ do
    return x # set children [] #+ i
