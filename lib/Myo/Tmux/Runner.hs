module Myo.Tmux.Runner where

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.Views (Views, ViewsError)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Functor (void)
import Ribosome.Control.Monad.Ribo (MonadRibo)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Runner (RunInIO, addRunner, extractRunError)
import Myo.Data.Env (Env)
import Myo.Tmux.Run (tmuxCanRun, tmuxCheckPending, tmuxRun)

addTmuxRunner ::
  MonadRibo m =>
  NvimE e m =>
  MonadIO m =>
  RunTmux m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepState s Views m =>
  MonadDeepError e RunError m =>
  MonadDeepError e ViewsError m =>
  MonadDeepState s CommandState m =>
  RunInIO m =>
  m ()
addTmuxRunner =
  void $ addRunner (Str "tmux") (extractRunError tmuxRun) (extractRunError tmuxCheckPending) tmuxCanRun
