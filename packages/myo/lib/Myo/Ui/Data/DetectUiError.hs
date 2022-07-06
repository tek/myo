module Myo.Ui.Data.DetectUiError where

import Exon (exon)
import Log (Severity (Debug, Error))
import Ribosome (ErrorMessage (ErrorMessage), ToErrorMessage (toErrorMessage))

data DetectUiError =
  VimPaneNotFound
  |
  Unexpected Text
  deriving stock (Eq, Show)

instance ToErrorMessage DetectUiError where
  toErrorMessage = \case
    VimPaneNotFound ->
      ErrorMessage msg [msg] Debug
      where
        msg =
          "Could not find Neovim's tmux pane by its process ID"
    Unexpected msg ->
      ErrorMessage [exon|Error when determining Neovim's tmux pane: #{msg}|] ["DetectUiError.Unexpected", msg] Error
