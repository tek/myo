module Myo.Ui.Focus where

import Chiasma.Command.Pane (selectPane)
import Chiasma.Data.Views (Views, ViewsError)
import qualified Chiasma.View.State as Views (paneId)
import Ribosome.Tmux.Run (RunTmux, runRiboTmux)

myoFocus ::
  MonadDeepError e ViewsError m =>
  MonadDeepState s Views m =>
  RunTmux m =>
  Ident ->
  m ()
myoFocus =
  runRiboTmux . selectPane <=< Views.paneId
