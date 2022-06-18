module Myo.Ui.Data.ToggleError where

import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Log (Severity (Error))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

data ToggleError =
  Tmux TmuxError
  |
  Render RenderError
  |
  Tree TreeModError
  deriving stock (Eq, Show, Generic)

instance ToErrorMessage ToggleError where
  toErrorMessage = \case
    Tmux e ->
      ErrorMessage "tmux error" ["ToggleError.Tmux:", show e] Error
    Render e ->
      ErrorMessage "tmux error" ["ToggleError.Render:", show e] Error
    Tree e ->
      ErrorMessage "tmux error" ["ToggleError.Tree:", show e] Error
