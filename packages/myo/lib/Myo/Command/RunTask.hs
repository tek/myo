module Myo.Command.RunTask where

import Myo.Command.Data.Command (Command (..))
import qualified Myo.Command.Data.Command as Command (ident)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.LogDir (LogDir)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (..))
import Myo.Command.Log (commandLogPath)
import Myo.Command.RunTaskDetails (runDetails)

runTask ::
  Members [Reader LogDir, AtomicState CommandState, Stop RunError, Stop RunError, Stop CommandError] r =>
  Command ->
  Sem r RunTask
runTask cmd = do
  details <- runDetails cmd
  cmdLog <- commandLogPath (Command.ident cmd)
  pure $ RunTask cmd cmdLog details
