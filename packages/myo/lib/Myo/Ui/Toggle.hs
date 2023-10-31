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
import Ribosome (Handler, Rpc, RpcError, mapReport, resumeReport)

import qualified Myo.Data.ViewError as ViewError
import Myo.Data.ViewError (ViewError)
import Myo.Orphans ()
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Lens.Toggle (hideOneLayout, hideOnePane, openOneLayout, openOnePane, toggleOneLayout, toggleOnePane)
import Myo.Ui.Render (renderTmux)

type ToggleStack encode decode =
  [
    ScopedTmux encode decode !! TmuxError,
    Codec TmuxCommand encode decode !! CodecError,
    Codec (Panes Pane) encode decode !! CodecError
  ]

myoOpenPane ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Rpc !! RpcError] r =>
  Ident ->
  Handler r ()
myoOpenPane ident =
  resumeReport @Rpc $ mapReport @ToggleError $ mapReport @ViewError do
    openOnePane ident
    resumeHoist ViewError.TmuxApi (mapStop ViewError.Render renderTmux)

myoOpenLayout ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Rpc !! RpcError] r =>
  Ident ->
  Handler r ()
myoOpenLayout ident =
  resumeReport @Rpc $ mapReport @ToggleError $ mapReport @ViewError do
    openOneLayout ident
    resumeHoist ViewError.TmuxApi (mapStop ViewError.Render renderTmux)

myoTogglePane ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Rpc !! RpcError] r =>
  Ident ->
  Handler r ()
myoTogglePane ident =
  resumeReport @Rpc $ mapReport @ToggleError $ mapReport @ViewError do
    toggleOnePane ident
    resumeHoist ViewError.TmuxApi (mapStop ViewError.Render renderTmux)

myoToggleLayout ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Rpc !! RpcError] r =>
  Ident ->
  Handler r ()
myoToggleLayout ident = do
  resumeReport @Rpc $ mapReport @ToggleError $ mapReport @ViewError do
    toggleOneLayout ident
    resumeHoist ViewError.TmuxApi (mapStop ViewError.Render renderTmux)

myoHidePane ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Rpc !! RpcError] r =>
  Ident ->
  Handler r ()
myoHidePane ident =
  resumeReport @Rpc $ mapReport @ToggleError $ mapReport @ViewError do
    hideOnePane ident
    resumeHoist ViewError.TmuxApi (mapStop ViewError.Render renderTmux)

myoHideLayout ::
  Members (ToggleStack encode decode) r =>
  Members [AtomicState Views, AtomicState UiState, Rpc !! RpcError] r =>
  Ident ->
  Handler r ()
myoHideLayout ident =
  resumeReport @Rpc $ mapReport @ToggleError $ mapReport @ViewError do
    hideOneLayout ident
    resumeHoist ViewError.TmuxApi (mapStop ViewError.Render renderTmux)
