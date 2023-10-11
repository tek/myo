module Myo.Test.Tmux.RunTest where

import Chiasma.Codec.Data (Pane)
import Chiasma.Codec.Data.PaneMode (PaneMode (PaneMode))
import Chiasma.Command.Pane (capturePane, copyMode, paneMode)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxId (PaneId (PaneId))
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (NativeCommandCodec)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux, withTmuxApis)
import Chiasma.TmuxApi (TmuxApi)
import Hedgehog.Internal.Property (Failure)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Test (Hedgehog, TestError, UnitTest, assert, assertJust)
import Ribosome (LogReport, SettingError, Settings, resumeReport, Rpc, RpcError)
import Ribosome.Test (assertWait, testHandler)

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (systemOptions)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Interpreter.Backend.Tmux (interpretBackendTmuxNoLog)
import Myo.Command.Run (myoRun)
import Myo.Data.CommandId (CommandId, commandIdText)
import Myo.Effect.Commands (Commands)
import Myo.Effect.Controller (Controller)
import Myo.Interpreter.Controller (interpretControllerTransient)
import Myo.Test.Embed (myoEmbedTmuxTest)
import Myo.Test.Tmux.Output (cleanLines)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Default (setupDefaultTestUi)
import Myo.Ui.Toggle (myoTogglePane)

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

ident :: CommandId
ident = "cmd"

setup ::
  Members [Settings !! SettingError, AtomicState UiState, Commands !! CommandError, Error TestError] r =>
  Member (AtomicState Views) r =>
  Sem r ()
setup =
  testHandler do
    resumeReport @Settings setupDefaultTestUi
    myoAddSystemCommand cmd
  where
    cmd =
      systemOptions ident cmds
      & #target ?~ "make"
    cmds = ["echo '" <> line1 <> "'", "echo '" <> line2 <> "'"]

runAndCheck ::
  Members [Rpc !! RpcError, DataLog LogReport, Hedgehog IO, Race, Embed IO] r =>
  Members [NativeTmux, NativeCommandCodec !! CodecError, Stop CodecError] r =>
  Members [Controller !! RunError, Commands !! CommandError, Error TestError, Error Failure, ChronosTime] r =>
  Sem r ()
runAndCheck = do
  testHandler (myoRun (commandIdText ident))
  withTmux $ restop $ assertWait (cleanLines <$> capturePane (PaneId 1)) \ out -> do
    assert (elem line1 out)
    assert (elem line2 out)

test_tmuxRunSys :: UnitTest
test_tmuxRunSys =
  myoEmbedTmuxTest $ interpretBackendTmuxNoLog $ interpretControllerTransient [] do
    setup
    runAndCheck

test_quitCopyMode :: UnitTest
test_quitCopyMode =
  myoEmbedTmuxTest $ interpretBackendTmuxNoLog $ interpretControllerTransient [] do
    setup
    testHandler (myoTogglePane "make")
    withTmuxApis @[TmuxCommand, Panes PaneMode, Panes Pane] $
      restop @CodecError $ restop @CodecError @(TmuxApi (Panes PaneMode)) do
        copyMode pid
        assertJust (PaneMode pid "copy-mode") =<< paneMode pid
    runAndCheck
  where
    pid =
      PaneId 1
