module Myo.Test.Run where

import Chiasma.Interpreter.Codec (interpretCodecPanes)
import Conc (interpretAtomic, interpretEventsChan)
import Log (Severity (Debug, Trace))
import Path (reldir)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, UnitTest)
import Ribosome (
  BootError,
  HandlerError,
  HostConfig,
  PluginConfig (PluginConfig),
  Rpc,
  RpcError,
  SettingError,
  Settings,
  noHandlers,
  setStderr,
  )
import Ribosome.Host.Data.HostConfig (HostConfig (HostConfig), dataLogConc)
import Ribosome.Test (StackWith, TestConfig (TestConfig), runEmbedTest, testHandler, testPluginEmbed)
import Ribosome.Test.Data.TestConfig (TmuxTestConfig (core))
import qualified Ribosome.Test.EmbedTmux as Tmux
import Ribosome.Test.EmbedTmux (testPluginEmbedTmuxConf)

import Myo.Command.Data.LogDir (LogDir (LogDir))
import Myo.Command.Interpreter.Executions (interpretExecutions)
import Myo.Interpreter.Proc (interpretProc)
import Myo.Plugin (MyoStack)

type MyoTestStack =
  Stop HandlerError : StackWith MyoStack

withLogDir ::
  Member Test r =>
  InterpreterFor (Reader LogDir) r
withLogDir sem = do
  d <- Test.tempDir [reldir|log|]
  runReader (LogDir d) sem

interpretMyoTestStack ::
  Member ChronosTime r =>
  Members [Test, Rpc !! RpcError, Settings !! SettingError, Error BootError, Race, Log, Resource, Async, Embed IO] r =>
  InterpretersFor MyoStack r
interpretMyoTestStack =
  interpretProc .
  interpretCodecPanes .
  interpretCodecPanes .
  withLogDir .
  interpretEventsChan .
  interpretAtomic def .
  interpretAtomic def .
  interpretAtomic def .
  interpretAtomic def .
  interpretExecutions

testConfig ::
  HostConfig ->
  TestConfig
testConfig (HostConfig conf) =
  TestConfig False (PluginConfig "myo" (HostConfig conf { dataLogConc = False }))

myoTestConf ::
  HasCallStack =>
  HostConfig ->
  Sem MyoTestStack () ->
  UnitTest
myoTestConf conf test =
  runEmbedTest (testConfig conf) do
    interpretMyoTestStack $ noHandlers $ testPluginEmbed $ testHandler do
      test

myoTest ::
  HasCallStack =>
  Sem MyoTestStack () ->
  UnitTest
myoTest =
  myoTestConf def

myoTestDebug ::
  HasCallStack =>
  Sem MyoTestStack () ->
  UnitTest
myoTestDebug =
  myoTestConf (setStderr Debug def)

type MyoTmuxTestStack =
  Stop HandlerError : Tmux.EmbedTmuxWith MyoStack

myoEmbedTmuxTestConf ::
  HasCallStack =>
  HostConfig ->
  Sem MyoTmuxTestStack () ->
  UnitTest
myoEmbedTmuxTestConf conf test =
  testPluginEmbedTmuxConf @MyoStack def { core = testConfig conf } (interpretMyoTestStack . noHandlers) do
    testHandler test

myoEmbedTmuxTest ::
  HasCallStack =>
  Sem MyoTmuxTestStack () ->
  UnitTest
myoEmbedTmuxTest =
  myoEmbedTmuxTestConf def

myoEmbedTmuxTestDebug ::
  HasCallStack =>
  Sem MyoTmuxTestStack () ->
  UnitTest
myoEmbedTmuxTestDebug =
  myoEmbedTmuxTestConf (setStderr Debug def)

myoEmbedTmuxTestTrace ::
  HasCallStack =>
  Sem MyoTmuxTestStack () ->
  UnitTest
myoEmbedTmuxTestTrace =
  myoEmbedTmuxTestConf (setStderr Trace def)
