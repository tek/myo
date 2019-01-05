module Myo.Test.Unit(
  spec,
  specWith,
  specWithDef,
  specConfig,
) where

import Data.Default.Class (def)
import UnliftIO.STM (newTVarIO)
import Ribosome.Test.Embed (Vars, TestConfig)
import Ribosome.Test.Unit (unitSpec)
import Myo.Data.Myo (Myo)
import Myo.Data.Env (Env)
import Myo.Test.Config (defaultTestConfig, defaultTestConfigWith)

specConfig :: TestConfig -> Env -> Myo () -> IO ()
specConfig conf e s = do
  t <- newTVarIO e
  unitSpec conf t s

spec :: Env -> Myo () -> IO ()
spec =
  specConfig defaultTestConfig

specWith :: Env -> Myo () -> Vars -> IO ()
specWith e s vars = do
  t <- newTVarIO e
  unitSpec (defaultTestConfigWith vars) t s

specWithDef :: Myo () -> Vars -> IO ()
specWithDef =
  specWith def
