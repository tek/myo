module Myo.Test.Handler where

import Log (Severity (Debug, Trace))
import Polysemy.Test (UnitTest)
import Ribosome (HostConfig, RpcHandler, mapHandlerError, rpcHandlers, setStderr)
import Ribosome.Host.Interpret (HigherOrder)
import Ribosome.Test (EmbedEffects, testPluginEmbed)
import Ribosome.Test.EmbedTmux (HandlerStack)

import Myo.Plugin (MyoStack)
import Myo.Test.Run (MyoTest, MyoTestStack, MyoTmuxTest, runMyoTestStack, runMyoTmuxTestStack)

myoTestHandlersConfWith ::
  ∀ r .
  HasCallStack =>
  HigherOrder r MyoTestStack =>
  HostConfig ->
  [RpcHandler (r ++ MyoTestStack)] ->
  InterpretersFor r MyoTestStack ->
  Sem (EmbedEffects ++ r ++ MyoTestStack) () ->
  UnitTest
myoTestHandlersConfWith conf handlers extra =
  runMyoTestStack conf .
  extra .
  rpcHandlers handlers .
  testPluginEmbed

myoTestHandlersWith ::
  ∀ r .
  HasCallStack =>
  HigherOrder r MyoTestStack =>
  [RpcHandler (r ++ MyoTestStack)] ->
  InterpretersFor r MyoTestStack ->
  Sem (EmbedEffects ++ r ++ MyoTestStack) () ->
  UnitTest
myoTestHandlersWith =
  myoTestHandlersConfWith @r def

myoTestHandlersConf ::
  HasCallStack =>
  HostConfig ->
  [RpcHandler MyoTestStack] ->
  Sem MyoTest () ->
  UnitTest
myoTestHandlersConf conf handlers =
  runMyoTestStack conf .
  rpcHandlers handlers .
  testPluginEmbed

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
  rpcHandlers handlers .
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
