module Myo.Tmux.Runner where

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.Views (Views, ViewsError)
import Control.Monad.DeepError (MonadDeepError, catchAt)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Functor (void)
import Ribosome.Data.Functor ((<$<))

import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import Myo.Command.Runner (RunInIO, addRunner, mkRunner)
import Myo.Data.Env (Env)
import Myo.Tmux.IO (RunTmux)
import Myo.Tmux.Run (tmuxCanRun, tmuxRun)

addTmuxRunner ::
  (MonadIO m, MonadBaseControl IO m, RunTmux m, MonadDeepState s Env m, MonadDeepState s Views m, MonadDeepError e RunError m, MonadDeepError e ViewsError m, MonadDeepState s Env m, MonadBaseControl IO m, RunInIO m) =>
  m ()
addTmuxRunner =
  void $ addRunner (Str "tmux") (mkRunner tmuxRun) tmuxCanRun
