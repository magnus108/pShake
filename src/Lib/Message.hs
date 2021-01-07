module Lib.Message
    ( Message(..)
    )
where

import qualified Lib.Model.Photographer as Photographer
import qualified Lib.Model.Tab as Tab
import qualified Lib.Model.Shooting as Shooting
import qualified Lib.Model.Dump as Dump
import qualified Lib.Model.Dagsdato as Dagsdato
import qualified Lib.Model.DagsdatoBackup as DagsdatoBackup
import qualified Lib.Model.Doneshooting as Doneshooting
import qualified Lib.Model.Camera as Camera
import qualified Lib.Model.Session as Session

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

    | ReadSessions
    | WriteSessions Session.Sessions
    | StartSessions
    | StopSessions

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

    | ReadDoneshooting
    | WriteDoneshooting Doneshooting.Doneshooting
    | StartDoneshooting
    | StopDoneshooting

    | ReadDagsdatoBackup
    | WriteDagsdatoBackup DagsdatoBackup.DagsdatoBackup
    | StartDagsdatoBackup
    | StopDagsdatoBackup

        deriving Show
