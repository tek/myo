module Myo.Quit where

import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (NativeCommandCodecE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Ribosome (Handler, mapReport)

import Myo.Tmux.Quit (closePanes)
import Myo.Ui.Data.UiState (UiState)

myoQuit ::
  Members [NativeTmux !! TmuxError, NativeCommandCodecE, AtomicState UiState, AtomicState Views] r =>
  Handler r ()
myoQuit =
  mapReport do
    closePanes
