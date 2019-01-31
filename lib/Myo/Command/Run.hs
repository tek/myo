module Myo.Command.Run(
  myoRun,
) where

import Chiasma.Data.Ident (Ident, sameIdent)
import Chiasma.Ui.Lens.Ident (matchIdentL)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (views, preview, set)
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.State.Class (MonadState, gets)
import Data.Maybe (isNothing)
import Neovim (Neovim)
import Ribosome.Control.Monad.RiboE (mapE, runRiboReport)
import Ribosome.Control.Monad.State (prepend)
import Ribosome.Control.Monad.State (riboState)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Msgpack.NvimObject (NO(..))

import Myo.Command.Command (commandByIdent)
import Myo.Command.Data.Command (Command(..))
import qualified Myo.Command.Data.CommandState as CommandState (_running)
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(..))
import Myo.Command.Data.RunTask (RunTask)
import Myo.Command.Data.RunningCommand (RunningCommand(RunningCommand))
import Myo.Command.History (pushHistory)
import Myo.Command.RunTask (runTask)
import Myo.Command.Runner (findRunner)
import Myo.Data.Env (Myo, MyoE, Runner(Runner), Env, Pid)
import qualified Myo.Data.Env as Env (_command)
import Myo.Orphans ()

runningLens :: Lens' Env [RunningCommand]
runningLens =
  Env._command . CommandState._running

findRunningCommand ::
  MonadState Env m =>
  Ident ->
  m (Maybe RunningCommand)
findRunningCommand ident =
  gets $ Lens.preview $ runningLens . matchIdentL ident

addRunningCommand ::
  MonadState Env m =>
  Ident ->
  Maybe Pid ->
  m ()
addRunningCommand ident pid =
  prepend runningLens (RunningCommand ident pid)

storeRunningCommand ::
  MonadState Env m =>
  Command ->
  Maybe Pid ->
  m ()
storeRunningCommand (Command _ ident _ _) pid = do
  existing <- findRunningCommand ident
  when (isNothing existing) $ addRunningCommand ident pid

executeRunner :: Runner -> RunTask -> MyoE RunError (Maybe Pid)
executeRunner (Runner _ _ run) = run

runCommand ::
  Command -> MyoE RunError ()
runCommand cmd = do
  task <- riboState $ runTask cmd
  runner <- riboState $ findRunner task
  pid <- executeRunner runner task
  riboState $ storeRunningCommand cmd pid
  riboState $ pushHistory cmd

runIdent ::
  Ident ->
  MyoE RunError ()
runIdent ident = do
  cmd <- mapE RunError.Command cmdResult
  runCommand cmd
  where
    cmdResult = riboState $ commandByIdent ident

myoRun :: NO Ident -> Myo ()
myoRun (NO ident) =
  runRiboReport "command" $ runIdent ident
