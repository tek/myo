module Myo.Test.ConfigTest where

import Hedgehog ((===))
import Ribosome.Config.Setting (setting, Settings.update)
import Ribosome.Test.Run (UnitTest)

import Myo.Proteome (myoProteomeLoaded)
import qualified Myo.Settings as Settings (proteomeMainType, testShell)
import Myo.Test.Unit (MyoTest, testDef)

proteomeConfigTest :: MyoTest ()
proteomeConfigTest = do
  Settings.update Settings.proteomeMainType "scala"
  myoProteomeLoaded
  ("sbt" ===) =<< setting Settings.testShell

test_proteomeConfig :: UnitTest
test_proteomeConfig =
  testDef proteomeConfigTest
