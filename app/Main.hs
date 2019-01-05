import Neovim

import Myo.Plugin (plugin)

main :: IO ()
main = neovim defaultConfig {plugins = [plugin]}
