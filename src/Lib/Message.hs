module Lib.Message
    ( Message(..)
    )
where

import qualified Lib.Model.Photographer as Photographer
import qualified Lib.Model.Tab as Tab
import qualified Lib.Model.Shooting as Shooting
import qualified Lib.Model.Dump as Dump

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

    | ReadDump
    | WriteDump Dump.Dump
    | StartDump
    | StopDump
        deriving Show
