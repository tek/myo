{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Tmux.RunShellSpec (htf_thisModulesTests) where

import Chiasma.Command.Pane (capturePane)
import Chiasma.Data.TmuxId (PaneId(PaneId))
import Prelude hiding (tmuxSpecDef)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Await (await)
import Ribosome.Tmux.Run (runTmux)
import Test.Framework

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions(AddShellCommandOptions))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Execution (isCommandRunning)
import Myo.Command.Kill (killCommand)
import Myo.Command.Run (myoRun)
import Myo.Data.Env (Myo)
import qualified Myo.Settings as Settings (processTimeout)
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Default (setupDefaultTestUi)
import Unit (tmuxSpecDef)

line1 :: Text
line1 =
  "shell command"

line2 :: Text
line2 =
  "with two cmdlines"

cmdLines :: [Text]
cmdLines =
  [line1, line2]

firstCondition ::
  AssertM m =>
  [Text] ->
  m ()
firstCondition output = do
  gassertEqual ("cat" : cmdLines) (take 3 output)
  gassertEqual cmdLines (drop 4 output)

secondCondition ::
  AssertM m =>
  [Text] ->
  m ()
secondCondition output =
  gassertEqual (cmdLines ++ cmdLines) (drop 6 output)

thirdCondition ::
  AssertM m =>
  [Text] ->
  m ()
thirdCondition output =
  gassertEqual (cmdLines ++ cmdLines) (drop 12 output)

tmuxRunShellSpec :: Myo ()
tmuxRunShellSpec = do
  let shellIdent = "cat"
  let cmdIdent = "text"
  updateSetting Settings.processTimeout 2
  addTmuxRunner
  setupDefaultTestUi
  myoAddSystemCommand $ AddSystemCommandOptions shellIdent ["cat"] (Just "tmux") (Just "make") Nothing Nothing
  myoAddShellCommand $ AddShellCommandOptions cmdIdent cmdLines (Just "tmux") shellIdent Nothing Nothing
  myoRun cmdIdent
  await gassertBool (isCommandRunning shellIdent)
  await firstCondition paneContent
  myoRun cmdIdent
  await secondCondition paneContent
  killCommand shellIdent
  await (gassertBool . not) (isCommandRunning shellIdent)
  myoRun cmdIdent
  await gassertBool (isCommandRunning shellIdent)
  await thirdCondition paneContent
  where
    paneContent =
      runTmux $ capturePane (PaneId 1)

test_tmuxRunShell :: IO ()
test_tmuxRunShell =
  tmuxSpecDef tmuxRunShellSpec
