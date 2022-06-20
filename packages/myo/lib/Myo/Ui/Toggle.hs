module Myo.Ui.Toggle where

import Chiasma.Codec.Data (Pane)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (Codec)
import Chiasma.Effect.TmuxClient (ScopedTmux)
import Ribosome (Handler, Rpc, mapHandlerError)

import Myo.Orphans ()
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Lens.Toggle (openOnePane, toggleOneLayout, toggleOnePane)
import Myo.Ui.Render (renderTmux)

type ToggleStack encode decode =
  [
    ScopedTmux () encode decode,
    Codec TmuxCommand encode decode !! CodecError,
    Codec (Panes Pane) encode decode !! CodecError
  ]

ensurePaneOpen ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Stop RenderError, Rpc, Stop ToggleError] r =>
  Ident ->
  Sem r ()
ensurePaneOpen ident = do
  openOnePane ident
  renderTmux

myoTogglePane ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Stop RenderError, Rpc] r =>
  Ident ->
  Handler r ()
myoTogglePane ident =
  mapHandlerError @ToggleError do
    toggleOnePane ident
    void renderTmux

myoToggleLayout ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Stop RenderError, Rpc] r =>
  Ident ->
  Handler r ()
myoToggleLayout ident = do
  mapHandlerError @ToggleError do
    toggleOneLayout ident
    void renderTmux
