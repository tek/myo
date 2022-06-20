module Myo.Proteome where

-- import qualified Chiasma.Data.Ident as Ident (Ident (Str))
-- import Data.Map ((!?))
-- import qualified Data.Map as Map (fromList)
-- import Ribosome.Api.Autocmd (uautocmd)
-- import Ribosome.Config.Setting (Settings.maybe, settingOr, Settings.update)
-- import Ribosome.Data.Setting (Setting)

-- import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions (AddShellCommandOptions))
-- import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (AddSystemCommandOptions))
-- import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec (CommandSettingCodec))
-- import qualified Myo.Settings as Settings
-- import Myo.Ui.Data.AddPaneOptions (AddPaneOptions)
-- import qualified Myo.Ui.Data.AddPaneOptions as AddPaneOptions (AddPaneOptions (..))
-- import Myo.Ui.Data.UiSettingCodec (UiSettingCodec (UiSettingCodec))

-- unset ::
--   MsgpackDecode a =>
--   Setting a ->
--   m Bool
-- unset =
--   fmap isNothing <$> Settings.maybe

-- haskellPanes :: [AddPaneOptions]
-- haskellPanes =
--   [
--     def {
--       AddPaneOptions.layout = "make",
--       AddPaneOptions.ident = Just "ghci",
--       AddPaneOptions.minSize = Just 0.5,
--       AddPaneOptions.maxSize = Just 35,
--       AddPaneOptions.position = Just 8
--     }
--   ]

-- stackCommand :: Text -> Text -> AddSystemCommandOptions
-- stackCommand cmd line =
--   AddSystemCommandOptions ident lines' Nothing (Just "make") (Just "haskell") Nothing Nothing Nothing Nothing
--   where
--     ident =
--       Ident.Str ("stack-" <> cmd)
--     lines' =
--       ["stack " <> line]

-- stackBuildCommand :: Text -> Text -> Bool -> AddSystemCommandOptions
-- stackBuildCommand cmd pro pedantic =
--   stackCommand (cmd <> suf) line
--   where
--     line =
--       cmd <> " --fast " <> pedanticArg <> pro
--     pedanticArg =
--       if pedantic then "--pedantic " else ""
--     suf =
--       if pedantic then "" else "-lenient"

-- haskellSystemCommands :: Bool -> Text -> Text -> [AddSystemCommandOptions]
-- haskellSystemCommands stack pro testPro =
--   AddSystemCommandOptions "ghci" ["ghci"] Nothing (Just "ghci") (Just "haskell") Nothing (Just True) Nothing Nothing :
--   if stack then [
--     stackBuildCommand "build" pro True,
--     stackBuildCommand "build" pro False,
--     stackBuildCommand "test" testPro True,
--     stackBuildCommand "test" testPro False,
--     stackCommand "clean" ("clean " <> pro),
--     stackCommand "clean-all" "clean"
--   ] else []

-- haskellConfig ::
--   m ()
-- haskellConfig = do
--   pro <- fromMaybe "" <$> Settings.maybe Settings.haskellCompileProject
--   testPro <- fromMaybe pro <$> Settings.maybe Settings.haskellTestProject
--   stack <- settingOr True Settings.haskellStack
--   whenM (unset Settings.ui) (set stack pro testPro)
--   where
--     set stack pro testPro =
--       Settings.update Settings.ui (UiSettingCodec Nothing (Just haskellPanes)) *>
--       Settings.update Settings.commands (CommandSettingCodec (Just (haskellSystemCommands stack pro testPro)) Nothing) *>
--       Settings.update Settings.testLang "haskell"

-- scalaPanes :: [AddPaneOptions]
-- scalaPanes =
--   [
--     def {
--       AddPaneOptions.layout = "make",
--       AddPaneOptions.ident = Just "sbt",
--       AddPaneOptions.minSize = Just 0.5,
--       AddPaneOptions.maxSize = Just 35,
--       AddPaneOptions.position = Just 8
--     }
--   ]

-- scalaSystemCommands :: [AddSystemCommandOptions]
-- scalaSystemCommands =
--   [AddSystemCommandOptions "sbt" ["sbt"] Nothing (Just "sbt") (Just "scala") Nothing (Just True) Nothing Nothing]

-- scalaShellCommands :: [AddShellCommandOptions]
-- scalaShellCommands =
--   [
--     AddShellCommandOptions "compile" ["test:compile"] Nothing "sbt" (Just "scala") Nothing Nothing Nothing Nothing,
--     AddShellCommandOptions "test" ["test"] Nothing "sbt" (Just "scala") Nothing Nothing Nothing Nothing,
--     AddShellCommandOptions "clean" ["clean"] Nothing "sbt" (Just "scala") Nothing (Just True) Nothing Nothing
--     ]

-- scalaConfig ::
--   m ()
-- scalaConfig =
--   whenM (unset Settings.ui) set
--   where
--     set =
--       Settings.update Settings.ui (UiSettingCodec Nothing (Just scalaPanes)) *>
--       Settings.update Settings.commands (CommandSettingCodec (Just scalaSystemCommands) (Just scalaShellCommands)) *>
--       Settings.update Settings.testShell "sbt" *>
--       Settings.update Settings.testLang "scala"

-- builtins ::
--   Map Text (m ())
-- builtins =
--   Map.fromList [
--     ("scala", scalaConfig),
--     ("haskell", haskellConfig)
--     ]

-- configureFromProteome ::
--   Text ->
--   m ()
-- configureFromProteome =
--   traverse_ id . (builtins !?)

-- myoProteomeLoaded ::
--   m ()
-- myoProteomeLoaded = do
--   whenM (settingOr True Settings.defaultCommands) run
--   uautocmd False "MyoBuiltinsLoaded"
--   where
--     run =
--       traverse_ configureFromProteome =<< Settings.maybe Settings.proteomeMainType
