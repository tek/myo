module Myo.Test.Embed where

import Log (Severity (Debug, Trace))
import Polysemy.Test (UnitTest)
import Ribosome (HostConfig, mapHandlerError, noHandlers, setStderr)
import Ribosome.Test (testPluginEmbed)

import Myo.Test.Run (MyoTest, MyoTmuxTest, runMyoTestStack, runMyoTmuxTestStack)

myoTestConf ::
  HasCallStack =>
  HostConfig ->
  Sem MyoTest () ->
  UnitTest
myoTestConf conf =
  runMyoTestStack conf .
  noHandlers .
  testPluginEmbed

myoTest ::
  HasCallStack =>
  Sem MyoTest () ->
  UnitTest
myoTest =
  myoTestConf def

myoTestDebug ::
  HasCallStack =>
  Sem MyoTest () ->
  UnitTest
myoTestDebug =
  myoTestConf (setStderr Debug def)

myoEmbedTmuxTestConf ::
  HasCallStack =>
  HostConfig ->
  Sem MyoTmuxTest () ->
  UnitTest
myoEmbedTmuxTestConf conf =
  runMyoTmuxTestStack conf .
  noHandlers .
  testPluginEmbed .
  mapHandlerError

myoEmbedTmuxTest ::
  HasCallStack =>
  Sem MyoTmuxTest () ->
  UnitTest
myoEmbedTmuxTest =
  myoEmbedTmuxTestConf def

myoEmbedTmuxTestDebug ::
  HasCallStack =>
  Sem MyoTmuxTest () ->
  UnitTest
myoEmbedTmuxTestDebug =
  myoEmbedTmuxTestConf (setStderr Debug def)

myoEmbedTmuxTestTrace ::
  HasCallStack =>
  Sem MyoTmuxTest () ->
  UnitTest
myoEmbedTmuxTestTrace =
  myoEmbedTmuxTestConf (setStderr Trace def)
