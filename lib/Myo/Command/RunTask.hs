module Myo.Command.RunTask(
  runTask,
) where

import Ribosome.Control.Monad.RiboE (mapE)
import Ribosome.Control.Monad.State (riboStateE)

import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(Toggle))
import Myo.Command.Data.RunTask (RunTask(..), RunTaskDetails)
import qualified Myo.Command.Data.RunTask as RunTaskDetails (RunTaskDetails(..))
import Myo.Command.Log (commandLog)
import Myo.Command.RunTaskDetails (runDetails)
import Myo.Data.Env (Env, MyoE)
import Myo.Ui.Toggle (ensurePaneOpen)

ensurePrerequisites ::
  RunTaskDetails ->
  MyoE RunError ()
ensurePrerequisites =
  ensure
  where
    ensure RunTaskDetails.Vim = return ()
    ensure (RunTaskDetails.UiSystem ident) = mapE RunError.Toggle $ ensurePaneOpen ident

runTask ::
  Command ->
  MyoE RunError RunTask
runTask cmd = do
  details <- riboStateE $ runDetails cmd
  ensurePrerequisites details
  cmdLog <- riboStateE $ commandLog (cmdIdent cmd)
  return $ RunTask cmd cmdLog details
