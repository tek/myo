import Neovim (neovim, defaultConfig, plugins)

import Myo.Env (bracketMyoTempDir)
import Myo.Plugin (plugin)

main :: IO ()
main =
  bracketMyoTempDir $ \dir -> neovim defaultConfig { plugins = [plugin dir] }
