module Myo.Ui.Toggle where

import Chiasma.Codec.Data (Pane)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (Codec)
import Chiasma.Effect.TmuxClient (ScopedTmux)
import Ribosome (Handler, Rpc, RpcError, mapHandlerError, resumeHandlerError)

import qualified Myo.Data.ViewError as ViewError
import Myo.Data.ViewError (ViewError)
import Myo.Orphans ()
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Lens.Toggle (openOnePane, toggleOneLayout, toggleOnePane)
import Myo.Ui.Render (renderTmux)

type ToggleStack encode decode =
  [
    ScopedTmux () encode decode !! TmuxError,
    Codec TmuxCommand encode decode !! CodecError,
    Codec (Panes Pane) encode decode !! CodecError
  ]

ensurePaneOpen ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Stop ViewError, Rpc, Stop ToggleError] r =>
  Ident ->
  Sem r ()
ensurePaneOpen ident = do
  openOnePane ident
  resumeHoist ViewError.TmuxApi (mapStop ViewError.Render renderTmux)

myoTogglePane ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Rpc !! RpcError] r =>
  Ident ->
  Handler r ()
myoTogglePane ident =
  resumeHandlerError @Rpc $ mapHandlerError @ToggleError $ mapHandlerError @ViewError do
    toggleOnePane ident
    resumeHoist ViewError.TmuxApi (mapStop ViewError.Render renderTmux)

myoToggleLayout ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Rpc !! RpcError] r =>
  Ident ->
  Handler r ()
myoToggleLayout ident = do
  resumeHandlerError @Rpc $ mapHandlerError @ToggleError $ mapHandlerError @ViewError do
    toggleOneLayout ident
    resumeHoist ViewError.TmuxApi (mapStop ViewError.Render renderTmux)
