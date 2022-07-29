module Myo.Test.Tmux.KillTest where

import Chiasma.Command.Pane (capturePane, sendKeys)
import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.SendKeysParams (Key (Lit))
import Chiasma.Data.TmuxCommand (TmuxCommand)
import Chiasma.Data.TmuxId (PaneId (PaneId))
import Chiasma.Effect.Codec (NativeCodecE)
import Chiasma.Effect.TmuxClient (NativeTmux)
import Chiasma.Tmux (withTmux_)
import Chiasma.TmuxNative (withTmuxNative_)
import Polysemy.Test (UnitTest, assert, evalMaybe, (/==))
import Ribosome (mapReport)
import qualified Ribosome.Settings as Settings
import Ribosome.Test (assertWait, testHandler, testHandlerAsync)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.CommandInterpreter (CommandInterpreter (System))
import Myo.Command.Data.TmuxTask (TaskType (Kill, Wait), TmuxTask (TmuxTask))
import qualified Myo.Command.Effect.Executions as Executions
import Myo.Command.Interpreter.Backend.Tmux (runInTmux)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Command.Interpreter.SocketReader (interpretSocketReader)
import Myo.Command.Interpreter.TmuxMonitor (interpretTmuxMonitor)
import qualified Myo.Settings as Settings (processTimeout)
import Myo.Test.Embed (myoEmbedTmuxTest)
import Myo.Test.Tmux.Output (cleanLines)

paneContent ::
  Members [NativeTmux, NativeCodecE TmuxCommand, Stop CodecError] r =>
  Sem r [Text]
paneContent =
  cleanLines <$> withTmux_ (capturePane (PaneId 1))

test_tmuxKill :: UnitTest
test_tmuxKill =
  myoEmbedTmuxTest $ interpretSocketReader $ interpretCommandLogSetting $ interpretTmuxMonitor $ testHandler do
    Settings.update Settings.processTimeout 2
    thread1 <- testHandlerAsync do
      mapReport (runInTmux (TmuxTask Wait "make" 0 cmd))
    assertWait (Executions.running ident) assert
    pid1 <- evalMaybe =<< Executions.pid ident
    withTmuxNative_ @TmuxCommand do
      sendKeys 0 [Lit "1"]
    thread2 <- testHandlerAsync do
      mapReport (runInTmux (TmuxTask Kill "make" 0 cmd))
    thread1
    assertWait (Executions.running ident) assert
    pid2 <- evalMaybe =<< Executions.pid ident
    pid1 /== pid2
    withTmuxNative_ @TmuxCommand do
      sendKeys 0 [Lit "2"]
    Executions.terminate ident
    thread2
    where
      cmd :: Command
      cmd =
        (Command.cons (System (Just "make")) ident ["cat"]) {runner, kill = True}
      ident =
        "cat"
      runner =
        Just "tmux"
