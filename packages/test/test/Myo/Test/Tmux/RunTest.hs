module Myo.Test.Tmux.RunTest where

import Chiasma.Codec.Data (Pane)
import Chiasma.Codec.Data.PaneMode (PaneMode (PaneMode))
import Chiasma.Command.Pane (capturePane, copyMode, paneMode)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (Ident (Str), identText)
import Chiasma.Data.Panes (Panes)
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxId (PaneId (PaneId))
import Chiasma.Data.Views (Views)
import Chiasma.Effect.Codec (NativeCommandCodec)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux, withTmuxApis)
import Hedgehog.Internal.Property (Failure)
import Polysemy.Chronos (ChronosTime)
import Polysemy.Test (Hedgehog, TestError, UnitTest, assert, assertJust)
import Ribosome (HostError, SettingError, Settings, resumeHandlerError)
import Ribosome.Test (assertWait, testHandler)

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (AddSystemCommandOptions))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Interpreter.Executor.Null (interpretExecutorNull)
import Myo.Command.Interpreter.Executor.Tmux (interpretExecutorTmux)
import Myo.Command.Run (myoRun)
import Myo.Effect.Controller (Controller)
import Myo.Interpreter.Controller (interpretController)
import Myo.Test.Run (myoEmbedTmuxTest)
import Myo.Test.Tmux.Output (cleanLines)
import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.Default (setupDefaultTestUi)
import Myo.Ui.Toggle (myoTogglePane)
import Chiasma.TmuxApi (TmuxApi)

line1 :: Text
line1 = "line 1"

line2 :: Text
line2 = "line 2"

ident :: Ident
ident = Str "cmd"

setup ::
  Members [Settings !! SettingError, AtomicState UiState, AtomicState CommandState, Error TestError] r =>
  Member (AtomicState Views) r =>
  Sem r ()
setup =
  testHandler do
    resumeHandlerError setupDefaultTestUi
    myoAddSystemCommand cmd
  where
    cmd =
      AddSystemCommandOptions ident cmds (Just (Str "tmux")) (Just (Str "make")) Nothing Nothing Nothing Nothing Nothing
    cmds = ["echo '" <> line1 <> "'", "echo '" <> line2 <> "'"]

runAndCheck ::
  Members [DataLog HostError, Hedgehog IO, Race, Embed IO] r =>
  Members [NativeTmux, NativeCommandCodec !! CodecError, Stop CodecError] r =>
  Members [Controller !! RunError, AtomicState CommandState, Error TestError, Error Failure, ChronosTime] r =>
  Sem r ()
runAndCheck = do
  testHandler (myoRun (identText ident))
  withTmux $ restop $ assertWait (cleanLines <$> capturePane (PaneId 1)) \ out -> do
    assert (elem line1 out)
    assert (elem line2 out)

test_tmuxRunSys :: UnitTest
test_tmuxRunSys =
  myoEmbedTmuxTest $ interpretExecutorNull $ interpretExecutorTmux $ interpretController do
    setup
    runAndCheck

test_quitCopyMode :: UnitTest
test_quitCopyMode =
  myoEmbedTmuxTest $ interpretExecutorNull $ interpretExecutorTmux $ interpretController do
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
