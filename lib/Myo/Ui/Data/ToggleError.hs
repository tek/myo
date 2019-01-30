module Myo.Ui.Data.ToggleError(
  ToggleError(..),
) where

import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxThunk (TmuxError)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Ribosome.Error.Report (ReportError(..))
import Myo.Ui.Error (tmuxErrorReport, renderErrorReport, treeModErrorReport)

data ToggleError =
  Tmux TmuxError
  |
  Render RenderError
  |
  Tree TreeModError
  deriving (Eq, Show)

instance ReportError ToggleError where
  errorReport (Tmux e) = tmuxErrorReport e
  errorReport (Render e) = renderErrorReport e
  errorReport (Tree e) = treeModErrorReport e
