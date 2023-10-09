module Myo.Test.Tmux.RunShellTest where

import Chiasma.Command.Pane (capturePane)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxId (PaneId (PaneId))
import Chiasma.Effect.Codec (NativeCodecE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux_)
import Polysemy.Test (Hedgehog, UnitTest, assert, assertEq, (===))
import qualified Ribosome.Settings as Settings
import Ribosome.Test (assertWait, testHandler, testHandlerAsync)

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import qualified Myo.Command.Data.AddShellCommandOptions as AddShellCommandOptions
import qualified Myo.Command.Data.AddSystemCommandOptions as AddSystemCommandOptions
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (commandShell, runner, target))
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Interpreter.Backend.Tmux (interpretBackendTmuxNoLog)
import Myo.Command.Proc (killCommand, terminateCommand)
import Myo.Command.Run (runIdent)
import Myo.Interpreter.Controller (interpretControllerTransient)
import qualified Myo.Settings as Settings (processTimeout)
import Myo.Test.Embed (myoEmbedTmuxTest)
import Myo.Test.Tmux.Output (cleanLines)
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

output1 :: [Text]
output1 =
  [
    line1,
    line1,
    line2,
    line2
  ]

firstCondition ::
  Member (Hedgehog IO) r =>
  [Text] ->
  Sem r ()
firstCondition out =
  output1 === dropWhile ("cat" ==) out

secondCondition ::
  Member (Hedgehog IO) r =>
  [Text] ->
  Sem r ()
secondCondition out =
  output1 ++ output1 === dropWhile ("cat" ==) out

thirdCondition ::
  Member (Hedgehog IO) r =>
  [Text] ->
  Sem r ()
thirdCondition out = do
  output1 ++ output1 ++ ["Terminated", "cat"] ++ output1 === dropWhile ("cat" ==) out

paneContent ::
  Members [NativeTmux, NativeCodecE TmuxCommand, Stop CodecError] r =>
  Sem r [Text]
paneContent =
  cleanLines <$> withTmux_ (capturePane (PaneId 1))

test_tmuxRunShell :: UnitTest
test_tmuxRunShell =
  myoEmbedTmuxTest $ interpretBackendTmuxNoLog $ interpretControllerTransient [] $ testHandler do
    Settings.update Settings.processTimeout 2
    setupDefaultTestUi
    myoAddSystemCommand (AddSystemCommandOptions.cons shellIdent ["cat"]) { runner, target = Just "make" }
    myoAddShellCommand (AddShellCommandOptions.cons cmdIdent cmdLines shellIdent) { AddShellCommandOptions.runner }
    runIdent cmdIdent mempty
    assertWait (Executions.running shellIdent) assert
    assertWait paneContent firstCondition
    runIdent cmdIdent mempty
    assertWait paneContent secondCondition
    terminateCommand shellIdent
    assertWait (Executions.running shellIdent) (assert . not)
    runIdent cmdIdent mempty
    assertWait (Executions.running shellIdent) assert
    assertWait paneContent thirdCondition
    where
      shellIdent =
        "cat"
      cmdIdent =
        "text"
      runner =
        Just "tmux"

test_tmuxUnixShell :: UnitTest
test_tmuxUnixShell =
  myoEmbedTmuxTest $ interpretBackendTmuxNoLog $ interpretControllerTransient [] $ testHandler do
    setupDefaultTestUi
    myoAddSystemCommand (AddSystemCommandOptions.cons shellIdent ["bash --norc"]) {
      runner,
      target = Just "make",
      commandShell = Just True
      }
    myoAddSystemCommand (AddSystemCommandOptions.cons cmdIdent ["echo text"]) { runner, target = Just "make" }
    runEchoInShell 2
    runEchoInShell 4
    where
      runEchoInShell n = do
        thread1 <- testHandlerAsync do
          runIdent shellIdent mempty
        assertWait (Executions.running shellIdent) assert
        runIdent cmdIdent mempty
        runIdent cmdIdent mempty
        assertWait paneContent (assertEq n . length . filter ("text" ==))
        killCommand shellIdent
        thread1
      shellIdent =
        "bash"
      cmdIdent =
        "echo"
      runner =
        Just "tmux"
