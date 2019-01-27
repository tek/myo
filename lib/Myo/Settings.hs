module Myo.Settings(
  vimTmuxPane,
  tmuxSocket,
) where

import Ribosome.Config.Setting

vimTmuxPane :: Setting Int
vimTmuxPane = Setting "vim_tmux_pane" True Nothing

tmuxSocket :: Setting FilePath
tmuxSocket = Setting "tmux_socket" True Nothing
