module Myo.Tmux.Runner where

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.Views (Views, ViewsError)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor (void)
import Ribosome.Control.Monad.Ribo (MonadRibo)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Runner (RunInIO, addRunner, mkRunner)
import Myo.Data.Env (Env)
import Myo.Tmux.Run (tmuxCanRun, tmuxRun)

addTmuxRunner ::
  MonadRibo m =>
  MonadIO m =>
  RunTmux m =>
  MonadDeepState s Env m =>
  MonadDeepState s Views m =>
  MonadDeepError e RunError m =>
  MonadDeepError e ViewsError m =>
  MonadDeepState s CommandState m =>
  RunInIO m =>
  m ()
addTmuxRunner =
  void $ addRunner (Str "tmux") (mkRunner tmuxRun) tmuxCanRun
