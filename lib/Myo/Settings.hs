module Myo.Settings where

import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(ViewGeometry))
import Ribosome.Data.Setting (Setting(Setting))

import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions)

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

outputSelectFirst :: Setting Bool
outputSelectFirst =
  Setting "output_jump_first" True (Just False)

outputAutoJump :: Setting Bool
outputAutoJump =
  Setting "output_auto_jump" True (Just True)

systemCommands :: Setting [AddSystemCommandOptions]
systemCommands =
  Setting "system_commands" True (Just [])

shellCommands :: Setting [AddShellCommandOptions]
shellCommands =
  Setting "shell_commands" True (Just [])

resetOnSave :: Setting Bool
resetOnSave =
  Setting "reset_on_save" True (Just True)
