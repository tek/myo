module Myo.Command.RunTask where

import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.LogDir (LogDir)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask (..))
import Myo.Command.RunTaskDetails (runDetails)

runTask ::
  Members [Reader LogDir, AtomicState CommandState, Stop RunError] r =>
  Command ->
  Sem r RunTask
runTask cmd = do
  details <- mapStop RunError.Command (runDetails cmd)
  pure (RunTask cmd details)
