module Myo.Test.Embed where

import Log (Severity (Debug, Trace))
import Polysemy.Test (UnitTest)
import Ribosome (HostConfig, mapHandlerError, setStderr)
import Ribosome.Test (testPluginEmbed)

import Myo.Test.Run (
  MyoSocketTmuxTest,
  MyoTest,
  MyoTmuxTest,
  runMyoSocketTmuxTestStack,
  runMyoTestStack,
  runMyoTmuxTestStack,
  )

myoTestConf ::
  HasCallStack =>
  HostConfig ->
  Sem MyoTest () ->
  UnitTest
myoTestConf conf =
  runMyoTestStack conf .
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
  testPluginEmbed .
  mapHandlerError

myoSocketTmuxTest ::
  HasCallStack =>
  Sem MyoSocketTmuxTest () ->
  UnitTest
myoSocketTmuxTest =
  myoSocketTmuxTestConf def
