module Myo.Tmux.Runner(
  addTmuxRunner,
) where

import Chiasma.Data.Ident (Ident(Str))
import Data.Functor (void)
import Ribosome.Control.Monad.State (runRiboStateE)

import Myo.Command.Runner (addRunner)
import Myo.Data.Env (Myo)
import Myo.Tmux.Run (tmuxRun, tmuxCanRun)

addTmuxRunner :: Myo ()
addTmuxRunner =
  void $ runRiboStateE $ addRunner (Str "tmux") tmuxRun tmuxCanRun
