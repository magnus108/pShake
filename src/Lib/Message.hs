module Lib.Message
    ( Message(..)
    )
where

import qualified Lib.Model.Photographer as Photographer
import qualified Lib.Model.Tab as Tab
import qualified Lib.Model.Shooting as Shooting
import qualified Lib.Model.Dump as Dump
import qualified Lib.Model.Dagsdato as Dagsdato
import qualified Lib.Model.Camera as Camera

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

    | ReadCameras
    | WriteCameras Camera.Cameras
    | StartCameras
    | StopCameras

    | ReadDump
    | WriteDump Dump.Dump
    | StartDump
    | StopDump

    | ReadDagsdato
    | WriteDagsdato Dagsdato.Dagsdato
    | StartDagsdato
    | StopDagsdato

        deriving Show
