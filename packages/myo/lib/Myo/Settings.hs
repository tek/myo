module Myo.Settings where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.ViewGeometry (ViewGeometry (ViewGeometry))
import Path (Dir, Path, Rel)
import Ribosome.Data.Setting (Setting (Setting))
import Time (Seconds)

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec)
import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Data.CommandId (CommandId)
import Myo.Ui.Data.UiSettingCodec (UiSettingCodec)

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

commands :: Setting CommandSettingCodec
commands =
  Setting "commands" True Nothing

maxLogSize :: Setting Int
maxLogSize =
  Setting "max_log_size" True (Just 10000)

ui :: Setting UiSettingCodec
ui =
  Setting "ui" True Nothing

resetOnSave :: Setting Bool
resetOnSave =
  Setting "reset_on_save" True (Just True)

vimTestFileNameModifier :: Setting Text
vimTestFileNameModifier =
  Setting "filename_modifier" True (Just ":.")

testRunner :: Setting Ident
testRunner =
  Setting "test_runner" True (Just "tmux")

testShell :: Setting CommandId
testShell =
  Setting "test_shell" True Nothing

testPane :: Setting UiTarget
testPane =
  Setting "test_pane" True (Just "make")

testLang :: Setting CommandLanguage
testLang =
  Setting "test_lang" True Nothing

testCapture :: Setting Bool
testCapture =
  Setting "test_capture" True Nothing

processTimeout :: Setting Int
processTimeout =
  Setting "process_timeout" True (Just 3)

saveBeforeRun :: Setting Bool
saveBeforeRun =
  Setting "save_before_run" True (Just True)

proteomeMainType :: Setting Text
proteomeMainType =
  Setting "proteome_main_type" False Nothing

proteomeMainTypeDir :: Setting (Path Rel Dir)
proteomeMainTypeDir =
  Setting "proteome_main_type" False Nothing

proteomeMainName :: Setting Text
proteomeMainName =
  Setting "proteome_main_name" False Nothing

proteomeMainNameDir :: Setting (Path Rel Dir)
proteomeMainNameDir =
  Setting "proteome_main_name" False Nothing

saveInterval :: Setting Seconds
saveInterval =
  Setting "save_interval" True (Just 1)
