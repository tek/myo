module Myo.Test.Handler where

import Log (Severity (Debug, Trace))
import Polysemy.Test (UnitTest)
import Ribosome (HostConfig, RpcHandler, mapHandlerError, setStderr)
import Ribosome.Host.Interpreter.Handlers (withHandlers)
import Ribosome.Test (testPluginConf, testPluginEmbed)
import Ribosome.Test.EmbedTmux (HandlerStack)

import Myo.Plugin (MyoStack)
import Myo.Test.Run (MyoTest, MyoTestStack, MyoTmuxTest, interpretMyoTestStack, runMyoTmuxTestStack, testConfig)

myoTestHandlersConf ::
  HasCallStack =>
  HostConfig ->
  [RpcHandler MyoTestStack] ->
  Sem MyoTest () ->
  UnitTest
myoTestHandlersConf conf handlers =
  testPluginConf @MyoStack (testConfig conf) interpretMyoTestStack handlers

myoTestHandlers ::
  HasCallStack =>
  [RpcHandler MyoTestStack] ->
  Sem MyoTest () ->
  UnitTest
myoTestHandlers =
  myoTestHandlersConf def

myoTestHandlersDebug ::
  HasCallStack =>
  [RpcHandler MyoTestStack] ->
  Sem MyoTest () ->
  UnitTest
myoTestHandlersDebug =
  myoTestHandlersConf (setStderr Debug def)

type MyoTmuxHandlerTestStack =
  MyoStack ++ HandlerStack

myoEmbedTmuxTestHandlersConf ::
  HasCallStack =>
  HostConfig ->
  [RpcHandler MyoTmuxHandlerTestStack] ->
  Sem MyoTmuxTest () ->
  UnitTest
myoEmbedTmuxTestHandlersConf conf handlers =
  runMyoTmuxTestStack conf .
  withHandlers handlers .
  testPluginEmbed .
  mapHandlerError

myoEmbedTmuxTestHandlers ::
  HasCallStack =>
  [RpcHandler MyoTmuxHandlerTestStack] ->
  Sem MyoTmuxTest () ->
  UnitTest
myoEmbedTmuxTestHandlers =
  myoEmbedTmuxTestHandlersConf def

myoEmbedTmuxTestHandlersDebug ::
  HasCallStack =>
  [RpcHandler MyoTmuxHandlerTestStack] ->
  Sem MyoTmuxTest () ->
  UnitTest
myoEmbedTmuxTestHandlersDebug =
  myoEmbedTmuxTestHandlersConf (setStderr Debug def)

myoEmbedTmuxTestHandlersTrace ::
  HasCallStack =>
  [RpcHandler MyoTmuxHandlerTestStack] ->
  Sem MyoTmuxTest () ->
  UnitTest
myoEmbedTmuxTestHandlersTrace =
  myoEmbedTmuxTestHandlersConf (setStderr Trace def)
