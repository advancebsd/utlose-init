module CLI.Commands
( startCommand
, stopCommand
, restartCommand
, statusCommand
, logsCommand
, lintCommand
, snapCommand
, revertCommand
, enableCommand
, disableCommand
, patchCommand
, queryCommand
) where

import Options.Applicative

import CLI.Types
import CLI.Commands.Managment
import CLI.Commands.Status
import CLI.Commands.Logs
import CLI.Commands.Lint
import CLI.Commands.Snap
import CLI.Commands.Revert
import CLI.Commands.Boot
import CLI.Commands.Patch
import CLI.Commands.Query
import CLI.Commands.Enable
import CLI.Commands.Disable


startCommand   = command "start"   (info mgrOptions     (progDesc "Start Service"))
stopCommand    = command "stop"    (info mgrOptions     (progDesc "Stop Service"))
restartCommand = command "restart" (info mgrOptions     (progDesc "Restart Service"))
statusCommand  = command "status"  (info statusOptions  (progDesc "Find the status of service(s)"))
logsCommand    = command "logs"    (info logsOptions    (progDesc "Get logs from services"))
lintCommand    = command "lint"    (info lintOptions    (progDesc "Generate errors and warning"))
snapCommand    = command "snap"    (info snapOptions    (progDesc "Create a snapshot of the system"))
revertCommand  = command "revert"  (info revertOptions  (progDesc "Rvert to previous snapshot"))
enableCommand  = command "enable"  (info enableOptions  (progDesc "Enable service(s) at boot"))
disableCommand = command "disable" (info disableOptions (progDesc "Disable service(s) at boot"))
patchCommand   = command "patch"   (info patchOptions   (progDesc "Safely add file to config"))
queryCommand   = command "query"   (info queryOptions   (progDesc "Query properties about the running system"))
