module Myo.Command.Run(
  myoRun,
  storeRunningCommand,
) where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Lens.Ident (matchIdentL)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (preview)
import Control.Monad (when)
import Control.Monad.DeepState (MonadDeepState, gets)
import Data.Maybe (isNothing)
import Ribosome.Control.Monad.Ribo (ConcNvimS, local, mapE, prepend)
import Ribosome.Error.Report (runRiboReport)
import Ribosome.Msgpack.NvimObject (NO(..))

import Myo.Command.Command (commandByIdent)
import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (running)
import Myo.Command.Data.Pid (Pid)
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(..))
import Myo.Command.Data.RunTask (RunTask)
import Myo.Command.Data.RunningCommand (RunningCommand(RunningCommand))
import Myo.Command.History (pushHistory)
import Myo.Command.RunTask (runTask)
import Myo.Command.Runner (findRunner)
import Myo.Data.Env (Env, MyoE, Runner(Runner))
import qualified Myo.Data.Env as Env (command)
import Myo.Orphans ()

findRunningCommand ::
  MonadDeepState s CommandState m =>
  Ident ->
  m (Maybe RunningCommand)
findRunningCommand ident =
  gets f
  where
    f :: CommandState -> Maybe RunningCommand
    f = Lens.preview $ CommandState.running . matchIdentL ident

addRunningCommand ::
  MonadDeepState s CommandState m =>
  Ident ->
  Maybe Pid ->
  m ()
addRunningCommand ident pid =
  prepend (CommandState.running :: Lens' CommandState [RunningCommand]) (RunningCommand ident pid)

storeRunningCommand ::
  MonadDeepState s CommandState m =>
  Command ->
  Maybe Pid ->
  m ()
storeRunningCommand (Command _ ident _ _ _) pid = do
  existing <- findRunningCommand ident
  when (isNothing existing) $ addRunningCommand ident pid

executeRunner :: Runner -> RunTask -> MyoE RunError (ConcNvimS Env) ()
executeRunner (Runner _ _ run) = run

runCommand :: Command -> MyoE RunError (ConcNvimS Env) ()
runCommand cmd = do
  task <- runTask cmd
  runner <- findRunner task
  executeRunner runner task
  -- storeRunningCommand cmd pid
  pushHistory cmd

runIdent ::
  Ident ->
  MyoE RunError (ConcNvimS Env) ()
runIdent ident = do
  cmd <- mapE RunError.Command cmdResult
  runCommand cmd
  where
    cmdResult = local Env.command $ commandByIdent ident

myoRun :: NO Ident -> ConcNvimS Env ()
myoRun (NO ident) =
  runRiboReport "command" thunk
  where
    thunk :: MyoE RunError (ConcNvimS Env) ()
    thunk = runIdent ident
