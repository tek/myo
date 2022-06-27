module Myo.Test.Tmux.CommandLogTest where

import Chiasma.Command.Pane (capturePane, sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.SendKeysParams (Key (Lit))
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxId (PaneId (PaneId))
import Chiasma.Effect.Codec (NativeCodecE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux_)
import Chiasma.TmuxNative (withTmuxNative_)
import qualified Data.Text as Text
import Polysemy.Test (UnitTest, assert, assertEq, assertJust)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (assertWait, testHandler, testHandlerAsync)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.CommandInterpreter (CommandInterpreter (System))
import Myo.Command.Data.TmuxTask (TaskType (Wait), TmuxTask (TmuxTask))
import qualified Myo.Command.Effect.CommandLog as CommandLog
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Interpreter.CommandLog (interpretCommandLog)
import Myo.Command.Interpreter.Executor.Tmux (interpretExecutorTmux)
import Myo.Command.Interpreter.SocketReader (interpretSocketReader)
import qualified Myo.Effect.Executor as Executor
import Myo.Effect.Executor (Executor)
import qualified Myo.Settings as Settings (processTimeout)
import Myo.Test.Run (myoEmbedTmuxTestDebug)
import Myo.Test.Tmux.Output (cleanLines)
import Myo.Ui.Default (setupDefaultTestUi)
import Data.Semigroup (Sum(Sum))
import qualified Data.ByteString as ByteString

paneContent ::
  Members [NativeTmux, NativeCodecE TmuxCommand, Stop CodecError] r =>
  Sem r [Text]
paneContent =
  cleanLines <$> withTmux_ (capturePane (PaneId 1))

test_tmuxTruncCommandLog :: UnitTest
test_tmuxTruncCommandLog =
  myoEmbedTmuxTestDebug $ interpretSocketReader $ interpretCommandLog 100 $ interpretExecutorTmux $ testHandler do
    Settings.update Settings.processTimeout 2
    setupDefaultTestUi
    thread1 <- testHandlerAsync do
      resumeAs @_ @(Executor _) (Just ["failed"]) (Executor.run (TmuxTask Wait 0 cmd))
    assertWait (Executions.running cmdIdent) assert
    withTmuxNative_ @TmuxCommand do
      sendKeys 0 [k]
      sendKeys 0 [k]
      sendKeys 0 [k]
    dbgs =<< CommandLog.chunks cmdIdent
    dbgs . fmap (foldMap (Sum . ByteString.length)) =<< CommandLog.chunks cmdIdent
    assertWait (fmap (take 7 . Text.lines) <$> CommandLog.get cmdIdent) (assertJust target)
    Executions.kill cmdIdent
    assertWait (Executions.running cmdIdent) (assert . not)
    assertEq Nothing =<< thread1
    where
      target =
        [l, l, l, l]
      k =
        Lit l
      l =
        Text.replicate 20 "x"
      cmd :: Command
      cmd =
        (Command.cons (System (Just "make")) cmdIdent ["cat"]) {runner}
      cmdIdent =
        "cat"
      runner =
        Just "tmux"
