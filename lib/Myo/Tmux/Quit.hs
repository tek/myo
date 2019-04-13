module Myo.Tmux.Quit where

import Chiasma.Command.Pane (closePane)
import Chiasma.Data.View (viewId)
import Chiasma.Data.Views (Views)
import qualified Chiasma.Data.Views as Views (panes)
import Control.Monad.DeepState (MonadDeepState, getsL)
import Data.Foldable (traverse_)
import Data.Maybe (catMaybes)
import Ribosome.Tmux.Run (RunTmux, runRiboTmux)

import Myo.Ui.Data.UiState (UiState)
import qualified Myo.Ui.Data.UiState as UiState (vimPaneId)

-- |Close all panes except for the one containing vim.
-- If no vim pane id was stored, no action will be performed.
closePanes ::
  RunTmux m =>
  MonadDeepState s UiState m =>
  MonadDeepState s Views m =>
  m ()
closePanes =
  traverse_ close =<< getsL @UiState UiState.vimPaneId
  where
    close vimPaneId = do
      panes <- getsL @Views Views.panes
      runRiboTmux $ traverse_ closePane (filter (vimPaneId /=) . catMaybes $ viewId <$> panes)

tmuxQuit ::
  RunTmux m =>
  MonadDeepState s UiState m =>
  MonadDeepState s Views m =>
  m ()
tmuxQuit =
  closePanes
