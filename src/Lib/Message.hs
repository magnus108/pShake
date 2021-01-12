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
import qualified Lib.Model.Location as Location
import qualified Lib.Model.Grade as Grade

data Message
    = ReadPhotographers
    | WritePhographers Photographer.Photographers
    | StartPhotograpers
    | StopPhotographers

    | ReadGrades
    | WriteGrades Grade.Grades
    | StartGrades
    | StopGrades

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

    | ReadLocation
    | WriteLocation Location.Location
    | StartLocation
    | StopLocation
        deriving Show
