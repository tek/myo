module Myo.Tmux.Runner where

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.Views (Views, ViewsError)
import Control.Monad.DeepError (MonadDeepError)
import Control.Monad.DeepState (MonadDeepState)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Functor (void)

import Myo.Command.Runner (addRunner)
import Myo.Data.Env (Env)
import Myo.Tmux.IO (RunTmux)
import Myo.Tmux.Run (tmuxCanRun, tmuxRun)

addTmuxRunner ::
  (MonadIO m, MonadBaseControl IO m, RunTmux m, MonadDeepState s Env m, MonadDeepState s Views m, MonadDeepError e ViewsError m, MonadDeepState s Env m, MonadBaseControl IO m) =>
  m ()
addTmuxRunner =
  void $ addRunner (Str "tmux") tmuxRun tmuxCanRun
