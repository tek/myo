module Myo.Tmux.Quit where

-- import Chiasma.Command.Pane (closePane)
-- import Chiasma.Data.View (viewId)
-- import Chiasma.Data.Views (Views)
-- import qualified Chiasma.Data.Views as Views (panes)
-- import Ribosome.Tmux.Run (RunTmux, runRiboTmux)

-- import Myo.Ui.Data.UiState (UiState)
-- import qualified Myo.Ui.Data.UiState as UiState (vimPaneId)

-- -- |Close all panes except for the one containing vim.
-- -- If no vim pane id was stored, no action will be performed.
-- closePanes ::
--   RunTmux m =>
--   Member (AtomicState Env) r =>
--   Member (AtomicState Env) r =>
--   m ()
-- closePanes =
--   traverse_ close =<< getL @UiState UiState.vimPaneId
--   where
--     close vimPaneId = do
--       panes <- getL @Views Views.panes
--       runRiboTmux $ traverse_ closePane (filter (vimPaneId /=) . catMaybes $ viewId <$> panes)

-- tmuxQuit ::
--   RunTmux m =>
--   Member (AtomicState Env) r =>
--   Member (AtomicState Env) r =>
--   m ()
-- tmuxQuit =
--   closePanes
