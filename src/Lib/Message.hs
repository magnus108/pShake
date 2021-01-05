module Lib.Message
    ( Message(..)
    )
where

import qualified Lib.Model.Photographer as Photographer
import qualified Lib.Model.Tab as Tab
import qualified Lib.Model.Shooting as Shooting

data Message
    = ReadPhotographers
    | WritePhographers Photographer.Photographers
    | StartPhotograpers
    | StopPhotographers

    | ReadTabs
    | WriteTabs Tab.Tabs
    | StartTabs
    | StopTabs

    | ReadShootings
    | WriteShootings Shooting.Shootings
    | StartShootings
    | StopShootings
        deriving Show
