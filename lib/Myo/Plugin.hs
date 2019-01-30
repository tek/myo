{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Myo.Plugin(
  plugin,
)
where

import UnliftIO.STM (TVar)
import Neovim (
  Plugin(..),
  Neovim,
  StartupConfig,
  NeovimConfig,
  NeovimPlugin,
  Synchronous(Async),
  command',
  function',
  wrapPlugin,
  )
import Ribosome.Control.Ribosome (Ribosome)
import Myo.Init (initialize)
import Myo.Data.Env (Env)
import Myo.Diag (myoDiag)
import Myo.Ui.Toggle (myoTogglePane, myoToggleLayout)
import Myo.Command.Run (myoRun)

plugin' :: Ribosome (TVar Env) -> Plugin (Ribosome (TVar Env))
plugin' env =
  Plugin {
    environment = env,
    exports = [
      $(command' 'myoDiag) [],
      $(function' 'myoTogglePane) Async,
      $(function' 'myoToggleLayout) Async,
      $(function' 'myoRun) Async
    ]
  }

plugin :: Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin = do
  env <- initialize
  wrapPlugin $ plugin' env
