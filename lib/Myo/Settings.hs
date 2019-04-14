module Myo.Settings where

import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(ViewGeometry))
import Ribosome.Data.Setting (Setting(Setting))

vimTmuxPane :: Setting Int
vimTmuxPane =
  Setting "vim_tmux_pane" True Nothing

displayResult :: Setting Bool
displayResult =
  Setting "display_result" True (Just True)

detectUi :: Setting Bool
detectUi =
  Setting "detect_ui" True (Just True)

defaultGeometry :: ViewGeometry
defaultGeometry =
  ViewGeometry Nothing Nothing (Just 130.0) Nothing (Just 0.5) Nothing

vimPaneGeometry :: Setting ViewGeometry
vimPaneGeometry =
  Setting "vim_pane_geometry" True (Just defaultGeometry)

outputJumpFirst :: Setting Bool
outputJumpFirst =
  Setting "output_jump_first" True (Just False)

outputAutoJump :: Setting Bool
outputAutoJump =
  Setting "output_auto_jump" True (Just True)
