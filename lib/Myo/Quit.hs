module Myo.Quit where

import Chiasma.Data.Views (Views)
import Control.Monad.DeepState (MonadDeepState)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Tmux.Quit (tmuxQuit)

myoQuit ::
  RunTmux m =>
  MonadDeepState s Views m =>
  m ()
myoQuit =
  tmuxQuit
