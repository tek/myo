module Myo.Quit where

import Chiasma.Data.Views (Views)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Tmux.Quit (tmuxQuit)
import Myo.Ui.Data.UiState (UiState)

myoQuit ::
  RunTmux m =>
  MonadDeepState s Views m =>
  MonadDeepState s UiState m =>
  m ()
myoQuit =
  tmuxQuit
