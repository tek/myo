{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Myo.Plugin(
  plugin,
)
where

import UnliftIO.STM (TVar)
import Neovim (
  Plugin(..),
  command',
  Neovim,
  StartupConfig,
  NeovimConfig,
  NeovimPlugin,
  wrapPlugin,
  )
import Ribosome.Control.Ribosome (Ribosome)
import Myo.Init (initialize)
import Myo.Data.Env (Env)
import Myo.Diag (myoDiag)

plugin' :: Ribosome (TVar Env) -> Plugin (Ribosome (TVar Env))
plugin' env =
  Plugin {
    environment = env,
    exports = [
      $(command' 'myoDiag) []
    ]
  }

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
  env <- initialize
  wrapPlugin $ plugin' env
