{-# LANGUAGE UndecidableInstances #-}

module Myo.Command.Run where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Chiasma.Ui.Lens.Ident (matchIdentL)
import Control.Lens (Lens')
import qualified Control.Lens as Lens (preview)
import Control.Monad (when)
import Control.Monad.DeepError (hoistEither, MonadDeepError)
import Control.Monad.DeepState (gets, MonadDeepState)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Maybe (isNothing)
import Ribosome.Control.Monad.Ribo (MonadRibo, prepend)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Command.Command (commandByIdent)
import Myo.Command.Data.Command (Command(..))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (running)
import Myo.Command.Data.Pid (Pid)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunningCommand (RunningCommand(RunningCommand))
import Myo.Command.Data.RunTask (RunTask)
import Myo.Command.History (pushHistory)
import Myo.Command.Runner (findRunner)
import Myo.Command.RunTask (runTask)
import Myo.Data.Env (Env, Runner(Runner))
import Myo.Orphans ()
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Render (MyoRender)

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

executeRunner :: (MonadIO m, MonadDeepError e RunError m, MonadDeepState s Env m) => Runner -> RunTask -> m ()
executeRunner (Runner _ _ run) task = do
  r <- liftIO $ run task
  hoistEither r

class (
    RunTmux m,
    MonadRibo m,
    MyoRender s e m,
    MonadDeepError e ToggleError m,
    MonadDeepError e TreeModError m,
    MonadDeepError e RunError m,
    MonadDeepState s Env m,
    MonadDeepState s CommandState m
    ) => MyoRun s e m where

instance (
    RunTmux m,
    MonadRibo m,
    MyoRender s e m,
    MonadDeepError e ToggleError m,
    MonadDeepError e TreeModError m,
    MonadDeepError e RunError m,
    MonadDeepState s Env m,
    MonadDeepState s CommandState m
    ) => MyoRun s e m where

runCommand :: MyoRun s e m => Command -> m ()
runCommand cmd = do
  task <- runTask cmd
  runner <- findRunner task
  executeRunner runner task
  -- storeRunningCommand cmd pid
  pushHistory cmd

myoRun :: (MyoRun s e m, MonadDeepError e CommandError m) => Ident -> m ()
myoRun ident = do
  cmd <- commandByIdent ident
  runCommand cmd
