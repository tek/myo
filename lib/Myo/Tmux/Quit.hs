module Myo.Tmux.Quit where

import Chiasma.Command.Pane (closePane)
import Chiasma.Data.View (viewId)
import Chiasma.Data.Views (Views)
import qualified Chiasma.Data.Views as Views (panes)
import Control.Monad.DeepState (MonadDeepState, getsL)
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Ribosome.Tmux.Run (RunTmux, runRiboTmux)

tmuxQuit ::
  RunTmux m =>
  MonadDeepState s Views m =>
  m ()
tmuxQuit = do
  panes <- getsL @Views Views.panes
  runRiboTmux $ traverse_ closePane (catMaybes $ viewId <$> panes)
