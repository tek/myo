module Myo.Proteome where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Data.Map ((!?))
import qualified Data.Map as Map (fromList)
import Ribosome.Config.Setting (settingMaybe, updateSetting)
import Ribosome.Data.Setting (Setting)

import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions(AddShellCommandOptions))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec(CommandSettingCodec))
import qualified Myo.Settings as Settings (
  commands,
  haskellCompileProject,
  haskellTestProject,
  proteomeMainType,
  testLang,
  testShell,
  ui,
  )
import Myo.Ui.Data.AddPaneOptions (AddPaneOptions)
import qualified Myo.Ui.Data.AddPaneOptions as AddPaneOptions (AddPaneOptions(..))
import Myo.Ui.Data.UiSettingCodec (UiSettingCodec(UiSettingCodec))

unset ::
  NvimE e m =>
  MonadRibo m =>
  MsgpackDecode a =>
  Setting a ->
  m Bool
unset =
  isNothing <$$> settingMaybe

haskellPanes :: [AddPaneOptions]
haskellPanes =
  [
    def {
      AddPaneOptions.layout = "make",
      AddPaneOptions.ident = Just "ghci",
      AddPaneOptions.minSize = Just 0.5,
      AddPaneOptions.maxSize = Just 35,
      AddPaneOptions.position = Just 8
    }
  ]

stackCommand :: Text -> Text -> AddSystemCommandOptions
stackCommand cmd line =
  AddSystemCommandOptions (Ident.Str ("stack-" <> cmd)) lines' Nothing (Just "make") (Just "haskell") Nothing
  where
    lines' =
      ["stack " <> line]

stackBuildCommand :: Text -> Text -> Bool -> AddSystemCommandOptions
stackBuildCommand cmd pro pedantic =
  stackCommand (cmd <> suf) line
  where
    line =
      cmd <> " --fast " <> pedanticArg <> pro
    pedanticArg =
      if pedantic then "--pedantic " else ""
    suf =
      if pedantic then "" else "-lenient"

haskellSystemCommands :: Text -> Text -> [AddSystemCommandOptions]
haskellSystemCommands pro testPro =
  [
    AddSystemCommandOptions "ghci" ["ghci"] Nothing (Just "ghci") (Just "haskell") Nothing,
    stackBuildCommand "build" pro True,
    stackBuildCommand "build" pro False,
    stackBuildCommand "test" testPro True,
    stackBuildCommand "test" testPro False,
    stackCommand "clean" ("clean " <> pro),
    stackCommand "clean-all" "clean"
    ]

haskellConfig ::
  NvimE e m =>
  MonadRibo m =>
  m ()
haskellConfig = do
  pro <- fromMaybe "" <$> settingMaybe Settings.haskellCompileProject
  testPro <- fromMaybe pro <$> settingMaybe Settings.haskellTestProject
  whenM (unset Settings.ui) (set pro testPro)
  where
    set pro testPro =
      updateSetting Settings.ui (UiSettingCodec Nothing (Just haskellPanes)) *>
      updateSetting Settings.commands (CommandSettingCodec (Just (haskellSystemCommands pro testPro)) Nothing) *>
      updateSetting Settings.testLang "haskell"

scalaPanes :: [AddPaneOptions]
scalaPanes =
  [
    def {
      AddPaneOptions.layout = "make",
      AddPaneOptions.ident = Just "sbt",
      AddPaneOptions.minSize = Just 0.5,
      AddPaneOptions.maxSize = Just 35,
      AddPaneOptions.position = Just 8
    }
  ]

scalaSystemCommands :: [AddSystemCommandOptions]
scalaSystemCommands =
  [AddSystemCommandOptions "sbt" ["sbt"] Nothing (Just "sbt") (Just "scala") Nothing]

scalaShellCommands :: [AddShellCommandOptions]
scalaShellCommands =
  [AddShellCommandOptions "compile" ["test:compile"] Nothing "sbt" (Just "scala") Nothing]

scalaConfig ::
  NvimE e m =>
  MonadRibo m =>
  m ()
scalaConfig =
  whenM (unset Settings.ui) set
  where
    set =
      updateSetting Settings.ui (UiSettingCodec Nothing (Just scalaPanes)) *>
      updateSetting Settings.commands (CommandSettingCodec (Just scalaSystemCommands) (Just scalaShellCommands)) *>
      updateSetting Settings.testShell "sbt" *>
      updateSetting Settings.testLang "scala"

builtins ::
  NvimE e m =>
  MonadRibo m =>
  Map Text (m ())
builtins =
  Map.fromList [
    ("scala", scalaConfig),
    ("haskell", haskellConfig)
    ]

configureFromProteome ::
  NvimE e m =>
  MonadRibo m =>
  Text ->
  m ()
configureFromProteome =
  traverse_ id . (builtins !?)

myoProteomeLoaded ::
  NvimE e m =>
  MonadRibo m =>
  m ()
myoProteomeLoaded =
  traverse_ configureFromProteome =<< settingMaybe Settings.proteomeMainType
