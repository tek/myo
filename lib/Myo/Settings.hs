module Myo.Settings(
  vimTmuxPane,
  tmuxSocket,
  displayResult,
) where

import Ribosome.Data.Setting (Setting(Setting))

vimTmuxPane :: Setting Int
vimTmuxPane = Setting "vim_tmux_pane" True Nothing

tmuxSocket :: Setting FilePath
tmuxSocket = Setting "tmux_socket" True Nothing

displayResult :: Setting Bool
displayResult = Setting "display_result" True (Just True)
