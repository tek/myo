module Myo.Test.Tmux.RunShellTest where

import Chiasma.Command.Pane (capturePane)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxId (PaneId (PaneId))
import Chiasma.Effect.Codec (NativeCodecE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux_)
import Polysemy.Test (Hedgehog, UnitTest, assert, (===))
import Ribosome (interpretPersistNull)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (assertWait, testHandler)

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import qualified Myo.Command.Data.AddShellCommandOptions as AddShellCommandOptions
import Myo.Command.Data.AddShellCommandOptions (AddShellCommandOptions (runner))
import qualified Myo.Command.Data.AddSystemCommandOptions as AddSystemCommandOptions
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (runner, target))
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Interpreter.Backend.Tmux (interpretBackendTmuxNoLog)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Command.Kill (killCommand)
import Myo.Command.Run (myoRunIdent)
import Myo.Interpreter.Controller (interpretController)
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
firstCondition out = do
  "cat" : output1 === out

secondCondition ::
  Member (Hedgehog IO) r =>
  [Text] ->
  Sem r ()
secondCondition out =
  "cat" : output1 ++ output1 === out

thirdCondition ::
  Member (Hedgehog IO) r =>
  [Text] ->
  Sem r ()
thirdCondition out = do
  "cat" : output1 ++ output1 ++ ["Terminated", "cat"] ++ output1 === out

paneContent ::
  Members [NativeTmux, NativeCodecE TmuxCommand, Stop CodecError] r =>
  Sem r [Text]
paneContent =
  cleanLines <$> withTmux_ (capturePane (PaneId 1))

test_tmuxRunShell :: UnitTest
test_tmuxRunShell =
  myoEmbedTmuxTest $ interpretPersistNull $ interpretCommandLogSetting $ interpretBackendTmuxNoLog $ interpretController $
  testHandler do
    Settings.update Settings.processTimeout 2
    setupDefaultTestUi
    myoAddSystemCommand (AddSystemCommandOptions.cons shellIdent ["cat"]) { runner, target = Just "make" }
    myoAddShellCommand (AddShellCommandOptions.cons cmdIdent cmdLines shellIdent) { runner }
    myoRunIdent cmdIdent
    assertWait (Executions.running shellIdent) assert
    assertWait paneContent firstCondition
    myoRunIdent cmdIdent
    assertWait paneContent secondCondition
    killCommand shellIdent
    assertWait (Executions.running shellIdent) (assert . not)
    myoRunIdent cmdIdent
    assertWait (Executions.running shellIdent) assert
    assertWait paneContent thirdCondition
    where
      shellIdent =
        "cat"
      cmdIdent =
        "text"
      runner =
        Just "tmux"
