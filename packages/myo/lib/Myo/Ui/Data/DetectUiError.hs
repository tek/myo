module Myo.Ui.Data.DetectUiError where

import Exon (exon)
import Log (Severity (Debug, Error))
import Ribosome (Report (Report), Reportable (toReport))

data DetectUiError =
  VimPaneNotFound
  |
  Unexpected Text
  deriving stock (Eq, Show)

instance Reportable DetectUiError where
  toReport = \case
    VimPaneNotFound ->
      Report msg [msg] Debug
      where
        msg =
          "Could not find Neovim's tmux pane by its process ID"
    Unexpected msg ->
      Report [exon|Error when determining Neovim's tmux pane: #{msg}|] ["DetectUiError.Unexpected", msg] Error
