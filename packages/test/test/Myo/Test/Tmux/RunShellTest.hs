module Myo.Test.Tmux.RunShellTest where

import Chiasma.Command.Pane (capturePane)
import Chiasma.Data.TmuxId (PaneId (PaneId))
import Polysemy.Test (UnitTest, assert, assertEq, assertJust, (===))
import qualified Ribosome.Settings as Settings
import Ribosome.Test (assertWait, testHandler)

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions (AddShellCommandOptions))
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (AddSystemCommandOptions))
-- import Myo.Command.Execution (isCommandRunning)
import Myo.Command.Interpreter.Executor.Null (interpretExecutorNull)
import Myo.Command.Interpreter.Executor.Tmux (interpretExecutorTmux)
-- import Myo.Command.Kill (killCommand)
import Myo.Command.Run (myoRunIdent)
import Myo.Interpreter.Controller (interpretController)
import qualified Myo.Settings as Settings (processTimeout)
import Myo.Test.Run (myoEmbedTmuxTest)
import Myo.Test.Tmux.Output (cleanLines)
import Myo.Ui.Default (setupDefaultTestUi)
import Chiasma.Tmux (withTmux)

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
  Sem r ()
firstCondition out = do
  assertJust "cat" (listToMaybe out)
  cmdLines === (takeEnd 2 out)

secondCondition ::
  [Text] ->
  Sem r ()
secondCondition out =
  (cmdLines ++ cmdLines) === takeEnd 4 out

thirdCondition ::
  [Text] ->
  Sem r ()
thirdCondition out = do
  15 === length out
  secondCondition out

test_tmuxRunShell :: UnitTest
test_tmuxRunShell =
  myoEmbedTmuxTest $ interpretExecutorNull $ interpretExecutorTmux $ interpretController $ testHandler do
    Settings.update Settings.processTimeout 2
    setupDefaultTestUi
    myoAddSystemCommand $ AddSystemCommandOptions shellIdent ["cat"] r (Just "make") Nothing Nothing Nothing Nothing Nothing
    myoAddShellCommand $ AddShellCommandOptions cmdIdent cmdLines r shellIdent Nothing Nothing Nothing Nothing Nothing
    myoRunIdent cmdIdent
    assertWait (isCommandRunning shellIdent) assert
    assertWait paneContent firstCondition
    myoRunIdent cmdIdent
    assertWait paneContent secondCondition
    killCommand shellIdent
    assertWait (isCommandRunning shellIdent) (assert . not)
    myoRunIdent cmdIdent
    assertWait (isCommandRunning shellIdent) assert
    assertWait paneContent thirdCondition
    where
      shellIdent =
        "cat"
      cmdIdent =
        "text"
      r =
        Just "tmux"
      paneContent =
        cleanLines <$> withTmux (capturePane (PaneId 1))
