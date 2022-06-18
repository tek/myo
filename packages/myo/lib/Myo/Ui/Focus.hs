module Myo.Ui.Focus where

import Chiasma.Command.Pane (selectPane)
import Chiasma.Data.Ident (Ident)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.Views (Views, ViewsError)
import Chiasma.Effect.Codec (Codec)
import Chiasma.Effect.TmuxClient (TmuxClient)
import Chiasma.Tmux (withTmux)
import qualified Chiasma.View.State as Views (paneId)

myoFocus ::
  ∀ err encode decode resource r .
  Members [AtomicState Views, Stop ViewsError] r =>
  Members [Scoped resource (TmuxClient encode decode), Codec TmuxCommand encode decode !! err, Stop err] r =>
  Ident ->
  Sem r ()
myoFocus i =
  withTmux $ restop do
    p <- Views.paneId i
    selectPane p
