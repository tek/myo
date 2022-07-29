module Myo.Ui.Focus where

import Chiasma.Command.Pane (selectPane)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (NativeCommandCodecE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux_)
import qualified Chiasma.View.State as Views (paneId)
import Ribosome (Handler, mapReport)

import Myo.Data.ViewError (codecError, resumeTmuxError, viewsError)

myoFocus ::
  Members [AtomicState Views, NativeTmux !! TmuxError, NativeCommandCodecE] r =>
  Ident ->
  Handler r ()
myoFocus i =
  mapReport $ codecError $ resumeTmuxError $ withTmux_ do
      selectPane =<< viewsError (Views.paneId i)
