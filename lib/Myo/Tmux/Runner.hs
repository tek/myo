module Myo.Tmux.Runner(
  addTmuxRunner,
) where

import Chiasma.Data.Ident (Ident(Str))
import Data.Functor (void)

import Myo.Command.Runner (addRunner)
import Myo.Data.Env (Myo)
import Myo.Tmux.Run (tmuxCanRun, tmuxRun)

addTmuxRunner :: Myo ()
addTmuxRunner =
  void $ addRunner (Str "tmux") tmuxRun tmuxCanRun
