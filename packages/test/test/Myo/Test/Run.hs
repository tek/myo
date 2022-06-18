module Myo.Test.Run where

import Conc (interpretAtomic, interpretEventsChan)
import Log (Severity (Debug))
import Path (reldir)
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

import Myo.Command.Data.LogDir (LogDir (LogDir))
import Myo.Plugin (MyoStack)

type MyoTestStack =
  Stop HandlerError : StackWith MyoStack

withLogDir ::
  Member Test r =>
  InterpreterFor (Reader LogDir) r
withLogDir sem = do
  d <- Test.tempDir [reldir|log|]
  runReader (LogDir d) sem

interpretMyoStackTest ::
  Members [Test, Rpc !! RpcError, Settings !! SettingError, Error BootError, Race, Log, Resource, Async, Embed IO] r =>
  InterpretersFor MyoStack r
interpretMyoStackTest =
  withLogDir .
  interpretEventsChan .
  interpretAtomic def .
  interpretAtomic def .
  interpretAtomic def

myoTestConf ::
  HasCallStack =>
  HostConfig ->
  Sem MyoTestStack () ->
  UnitTest
myoTestConf conf test =
  runEmbedTest (TestConfig False (PluginConfig "myo" conf)) do
    interpretMyoStackTest $ noHandlers $ testPluginEmbed $ testHandler do
      test

myoTest ::
  HasCallStack =>
  Sem MyoTestStack () ->
  UnitTest
myoTest =
  myoTestConf (HostConfig def { dataLogConc = False })

myoTestDebug ::
  HasCallStack =>
  Sem MyoTestStack () ->
  UnitTest
myoTestDebug =
  myoTestConf (setStderr Debug def)
