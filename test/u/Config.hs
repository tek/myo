module Config (
  module Config,
  module Myo.Settings,
) where

import Myo.Settings
import Ribosome.Data.Setting (Setting(Setting))
import Ribosome.Msgpack.Encode (MsgpackEncode(..))
import Ribosome.Test.Embed (TestConfig(tcVariables), Vars(..))
import qualified Ribosome.Test.Embed as E (defaultTestConfig, defaultTestConfigWith)

defaultVars :: IO Vars
defaultVars =
  return $ Vars []

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
  conf { tcVariables = tcVariables conf <> Vars [(name, toMsgpack val)] }

svar ::
  MsgpackEncode a =>
  Setting a ->
  a ->
  TestConfig ->
  TestConfig
svar (Setting name prefix _) =
  var (if prefix then "myo_" <> name else name)
