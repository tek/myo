module Myo.Tmux.Runner(
  addTmuxRunner,
) where

import Chiasma.Data.Ident (Ident(Str))
import Control.Monad.DeepState (MonadDeepState)
import Data.Functor (void)

import Myo.Command.Runner (addRunner)
import Myo.Data.Env (Env)
import Myo.Tmux.Run (tmuxCanRun, tmuxRun)

addTmuxRunner ::
  MonadDeepState s Env m =>
  m ()
addTmuxRunner =
  void $ addRunner (Str "tmux") tmuxRun tmuxCanRun
