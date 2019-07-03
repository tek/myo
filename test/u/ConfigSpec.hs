{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ConfigSpec (htf_thisModulesTests) where

import Ribosome.Config.Setting (setting, updateSetting)
import Ribosome.Data.Setting (Setting)
import Test.Framework

import Myo.Data.Env (Myo)
import Myo.Proteome (myoProteomeLoaded)
import qualified Myo.Settings as Settings (commands, proteomeMainType, testShell, ui)
import Unit (specDef)

proteomeConfigSpec :: Myo ()
proteomeConfigSpec = do
  updateSetting Settings.proteomeMainType "scala"
  myoProteomeLoaded
  gassertEqual "sbt" =<< setting Settings.testShell

test_proteomeConfig :: IO ()
test_proteomeConfig =
  specDef proteomeConfigSpec
