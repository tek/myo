module Config where

import Ribosome.Test.Embed (TestConfig, Vars(..))
import qualified Ribosome.Test.Embed as E (defaultTestConfig, defaultTestConfigWith)

vars :: IO Vars
vars =
  return $ Vars []

defaultTestConfigWith :: Vars -> TestConfig
defaultTestConfigWith = E.defaultTestConfigWith "myo"

defaultTestConfig :: TestConfig
defaultTestConfig = E.defaultTestConfig "myo"
