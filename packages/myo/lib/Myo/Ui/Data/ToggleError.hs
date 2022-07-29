module Myo.Ui.Data.ToggleError where

import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Log (Severity (Error))
import Ribosome (Report (Report), Reportable (toReport))

data ToggleError =
  Tmux TmuxError
  |
  Render RenderError
  |
  Tree TreeModError
  deriving stock (Eq, Show, Generic)

instance Reportable ToggleError where
  toReport = \case
    Tmux e ->
      Report "tmux error" ["ToggleError.Tmux:", show e] Error
    Render e ->
      Report "tmux error" ["ToggleError.Render:", show e] Error
    Tree e ->
      Report "tmux error" ["ToggleError.Tree:", show e] Error
