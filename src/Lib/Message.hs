module Lib.Message
    ( Message(..)
    )
where

import Lib.Model

data Message
    = ReadPhotographers
    | WritePhographers Photographers
    | StartPhotograpers
    | StopPhotographers
        deriving Show
