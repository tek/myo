module Myo.Settings where

import Chiasma.Ui.Data.ViewGeometry (ViewGeometry(ViewGeometry))
import Ribosome.Data.Setting (Setting(Setting))

import Myo.Command.Data.Command (CommandLanguage)
import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec)
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

testShell :: Setting Ident
testShell =
  Setting "test_shell" True Nothing

testPane :: Setting Ident
testPane =
  Setting "test_pane" True (Just "make")

testLang :: Setting CommandLanguage
testLang =
  Setting "test_lang" True Nothing

processTimeout :: Setting Int
processTimeout =
  Setting "process_timeout" True (Just 3)

saveBeforeRun :: Setting Bool
saveBeforeRun =
  Setting "save_before_run" True (Just True)

proteomeMainType :: Setting Text
proteomeMainType =
  Setting "proteome_main_type" False Nothing

proteomeMainName :: Setting Text
proteomeMainName =
  Setting "proteome_main_name" False Nothing

saveInterval :: Setting Double
saveInterval =
  Setting "save_interval" True (Just 1.0)

haskellCompileProject :: Setting Text
haskellCompileProject =
  Setting "haskell_compile_project" True Nothing

haskellTestProject :: Setting Text
haskellTestProject =
  Setting "haskell_test_project" True Nothing

defaultCommands :: Setting Bool
defaultCommands =
  Setting "default_commands" True (Just True)
