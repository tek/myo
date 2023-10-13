module Myo.Test.Handler where

import Log (Severity (Debug, Trace))
import Polysemy.Test (UnitTest)
import Ribosome (HostConfig, RpcHandler, mapReport, setStderr, withHandlers)
import Ribosome.Host.Interpret (HigherOrder)
import Ribosome.Test (EmbedHandlerStack, testPluginConf, testPluginEmbed)
import Ribosome.Test.EmbedTmux (HandlerStack)

import Myo.Plugin (MyoStack)
import Myo.Test.Run (
  MyoTestStack,
  MyoTestStackWith,
  MyoTestWith,
  MyoTmuxTest,
  interpretMyoTestStack,
  runMyoTmuxTestStack,
  testConfig,
  )

myoTestHandlersConf ::
  ∀ r .
  HasCallStack =>
  HigherOrder (r ++ MyoStack) EmbedHandlerStack =>
  HostConfig ->
  (∀ x . Sem (MyoTestStackWith r) x -> Sem MyoTestStack x) ->
  [RpcHandler (MyoTestStackWith r)] ->
  Sem (MyoTestWith r) () ->
  UnitTest
myoTestHandlersConf conf extra handlers =
  testPluginConf @(r ++ MyoStack) (testConfig conf) (interpretMyoTestStack . extra) handlers

myoTestHandlers ::
  ∀ r .
  HasCallStack =>
  HigherOrder (r ++ MyoStack) EmbedHandlerStack =>
  (∀ x . Sem (MyoTestStackWith r) x -> Sem MyoTestStack x) ->
  [RpcHandler (MyoTestStackWith r)] ->
  Sem (MyoTestWith r) () ->
  UnitTest
myoTestHandlers =
  myoTestHandlersConf @r def

myoTestHandlersDebug ::
  ∀ r .
  HasCallStack =>
  HigherOrder (r ++ MyoStack) EmbedHandlerStack =>
  (∀ x . Sem (MyoTestStackWith r) x -> Sem MyoTestStack x) ->
  [RpcHandler (MyoTestStackWith r)] ->
  Sem (MyoTestWith r) () ->
  UnitTest
myoTestHandlersDebug =
  myoTestHandlersConf @r (setStderr Debug def)

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
  mapReport

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
