module Myo.Test.Run where

import Chiasma.Interpreter.Codec (interpretCodecPanes)
import Conc (interpretAtomic, interpretEventsChan, interpretSyncAs)
import Path (reldir)
import Polysemy.Chronos (ChronosTime)
import qualified Polysemy.Test as Test
import Polysemy.Test (Test, UnitTest)
import Ribosome (BootError, HostConfig, PluginConfig (PluginConfig), Rpc, RpcError, SettingError, Settings)
import Ribosome.Host.Data.HostConfig (HostConfig (HostConfig), dataLogConc)
import Ribosome.Host.Interpret (with)
import Ribosome.Test (EmbedHandlerStack, EmbedStackWith, TestConfig (TestConfig), runEmbedTest)
import Ribosome.Test.Data.TestConfig (TmuxTestConfig (core))
import Ribosome.Test.EmbedTmux (EmbedTmuxWith, HandlerStack, runEmbedTmuxTestConf)

import Myo.Command.Data.LogDir (LogDir (LogDir))
import Myo.Command.Interpreter.Executions (interpretExecutions)
import Myo.Command.Interpreter.Executor.Generic (interpretExecutorFail)
import Myo.Data.SaveLock (SaveLock (SaveLock))
import Myo.Data.ViewError (ViewError)
import Myo.Interpreter.Proc (interpretProc)
import Myo.Plugin (MyoStack)

withLogDir ::
  Member Test r =>
  InterpreterFor (Reader LogDir) r
withLogDir =
  with (Test.tempDir [reldir|log|]) \ d -> runReader (LogDir d)

interpretMyoTestStack ::
  Member ChronosTime r =>
  Members [Test, Rpc !! RpcError, Settings !! SettingError, Error BootError, Race, Log, Resource, Async, Embed IO] r =>
  InterpretersFor MyoStack r
interpretMyoTestStack =
  interpretSyncAs SaveLock .
  interpretExecutorFail .
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

type MyoTestStack =
  MyoStack ++ EmbedHandlerStack

type MyoTestWith r =
  EmbedStackWith (r ++ MyoStack)

type MyoTest =
  EmbedStackWith MyoStack

runMyoTestStack ::
  HasCallStack =>
  HostConfig ->
  Sem MyoTestStack () ->
  UnitTest
runMyoTestStack conf =
  runEmbedTest (testConfig conf) .
  interpretMyoTestStack

type MyoTmuxTestStack =
  MyoStack ++ HandlerStack

type MyoTmuxTest =
  Stop ViewError : EmbedTmuxWith MyoStack

runMyoTmuxTestStack ::
  HasCallStack =>
  HostConfig ->
  Sem MyoTmuxTestStack () ->
  UnitTest
runMyoTmuxTestStack conf =
  runEmbedTmuxTestConf def { core = testConfig conf } .
  interpretMyoTestStack
