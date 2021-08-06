module Myo.Test.Tmux.RunShellTest where

import Chiasma.Command.Pane (capturePane)
import Chiasma.Data.TmuxId (PaneId (PaneId))
import Hedgehog ((===))
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Test.Await (await, awaitEqual_)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Tmux.Run (runTmux)

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions (AddShellCommandOptions))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (AddSystemCommandOptions))
import Myo.Command.Execution (isCommandRunning)
import Myo.Command.Kill (killCommand)
import Myo.Command.Run (myoRunIdent)
import qualified Myo.Settings as Settings (processTimeout)
import Myo.Test.Tmux.Output (cleanLines)
import Myo.Test.Unit (MyoTest, tmuxTestDef)
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Default (setupDefaultTestUi)

line1 :: Text
line1 =
  "shell command"

line2 :: Text
line2 =
  "with two cmdlines"

cmdLines :: [Text]
cmdLines =
  [line1, line2]

takeEnd :: Int -> [a] -> [a]
takeEnd count xs =
  drop (length xs - count) xs

firstCondition ::
  [Text] ->
  MyoTest ()
firstCondition output = do
  Just "cat" === listToMaybe output
  cmdLines === (takeEnd 2 output)

secondCondition ::
  [Text] ->
  MyoTest ()
secondCondition output =
  (cmdLines ++ cmdLines) === takeEnd 4 output

thirdCondition ::
  [Text] ->
  MyoTest ()
thirdCondition output = do
  15 === length output
  secondCondition output

tmuxRunShellTest :: MyoTest ()
tmuxRunShellTest = do
  updateSetting Settings.processTimeout 2
  lift addTmuxRunner
  setupDefaultTestUi
  myoAddSystemCommand $ AddSystemCommandOptions shellIdent ["cat"] r (Just "make") Nothing Nothing Nothing Nothing Nothing
  myoAddShellCommand $ AddShellCommandOptions cmdIdent cmdLines r shellIdent Nothing Nothing Nothing Nothing Nothing
  lift (myoRunIdent cmdIdent)
  awaitEqual_ True (isCommandRunning shellIdent)
  await firstCondition paneContent
  lift (myoRunIdent cmdIdent)
  await secondCondition paneContent
  killCommand shellIdent
  awaitEqual_ False (isCommandRunning shellIdent)
  lift (myoRunIdent cmdIdent)
  awaitEqual_ True (isCommandRunning shellIdent)
  await thirdCondition paneContent
  where
    shellIdent = "cat"
    cmdIdent = "text"
    r = Just "tmux"
    paneContent =
      cleanLines <$> runTmux (capturePane (PaneId 1))

test_tmuxRunShell :: UnitTest
test_tmuxRunShell =
  tmuxTestDef tmuxRunShellTest
