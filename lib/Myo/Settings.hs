module Myo.Settings(
  vimTmuxPane,
) where

import Ribosome.Config.Setting

vimTmuxPane :: Setting Int
vimTmuxPane = Setting "vim_tmux_pane" True Nothing
