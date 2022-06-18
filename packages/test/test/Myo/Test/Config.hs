module Myo.Test.Config (
  module Myo.Test.Config,
  module Myo.Settings,
) where

import Ribosome.Data.Setting (Setting(Setting))
import Ribosome.Test.Embed (TestConfig(tcVariables), Vars(..), varsFromList)
import qualified Ribosome.Test.Embed as E (defaultTestConfig, defaultTestConfigWith)

import Myo.Settings

defaultVars :: IO Vars
defaultVars =
  pure def

defaultTestConfigWith :: Vars -> TestConfig
defaultTestConfigWith = E.defaultTestConfigWith "myo"

defaultTestConfig :: TestConfig
defaultTestConfig = E.defaultTestConfig "myo"

testConf :: (TestConfig -> TestConfig) -> TestConfig
testConf f =
  f defaultTestConfig

var ::
  MsgpackEncode a =>
  Text ->
  a ->
  TestConfig ->
  TestConfig
var name val conf =
  conf { tcVariables = tcVariables conf <> varsFromList [(name, toMsgpack val)] }

svar ::
  MsgpackEncode a =>
  Setting a ->
  a ->
  TestConfig ->
  TestConfig
svar (Setting name prefix _) =
  var (if prefix then "myo_" <> name else name)
