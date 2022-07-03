module Myo.Test.Embed where

import Log (Severity (Debug, Trace))
import Polysemy.Test (UnitTest)
import Ribosome (HostConfig, mapHandlerError, noHandlers, setStderr)
import Ribosome.Test (testPluginEmbed)
import Ribosome.Test.SocketTmux (testPluginSocket)

import Myo.Test.Run (MyoTest, MyoTmuxTest, runMyoTestStack, runMyoTmuxTestStack, MyoSocketTmuxTest, runMyoSocketTmuxTestStack)

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

myoTestTrace ::
  HasCallStack =>
  Sem MyoTest () ->
  UnitTest
myoTestTrace =
  myoTestConf (setStderr Trace def)

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

myoSocketTmuxTestConf ::
  HasCallStack =>
  HostConfig ->
  Sem MyoSocketTmuxTest () ->
  UnitTest
myoSocketTmuxTestConf conf =
  runMyoSocketTmuxTestStack conf .
  noHandlers .
  testPluginSocket .
  mapHandlerError

myoSocketTmuxTest ::
  HasCallStack =>
  Sem MyoSocketTmuxTest () ->
  UnitTest
myoSocketTmuxTest =
  myoSocketTmuxTestConf def
