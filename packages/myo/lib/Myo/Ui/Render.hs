module Myo.Ui.Render where

import Chiasma.Codec.Data (Pane)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (Codec)
import Chiasma.Effect.TmuxClient (ScopedTmux)
import Chiasma.Render (render)
import Chiasma.Tmux (withTmuxApis)
import Ribosome (Rpc)
import Ribosome.Api (nvimCwd)

import Myo.Ui.Data.Space (Space (Space))
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Data.Window (Window (Window))
import Myo.Ui.Space (myoSpaces)

renderTmux ::
  Member (Codec TmuxCommand encode decode !! CodecError) r =>
  Member (Codec (Panes Pane) encode decode !! CodecError) r =>
  Members [ScopedTmux () encode decode, AtomicState Views, AtomicState UiState, Stop RenderError, Rpc] r =>
  Sem r ()
renderTmux = do
  cwd <- nvimCwd
  spaces <- myoSpaces
  withTmuxApis @[TmuxCommand, Panes _] do
    for_ spaces \ (Space spaceIdent windows) ->
      for_ windows \ (Window windowIdent tree) ->
        render cwd spaceIdent windowIdent tree
