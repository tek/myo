{-# LANGUAGE TemplateHaskell #-}

module Myo.Plugin(
  plugin,
)
where

import Neovim (
  Neovim,
  NeovimConfig,
  NeovimPlugin,
  Plugin(..),
  StartupConfig,
  Synchronous(Async),
  command',
  function',
  wrapPlugin,
  )
import Ribosome.Control.Ribosome (Ribosome)

import Myo.Command.Run (myoRun)
import Myo.Data.Env (Env)
import Myo.Diag (myoDiag)
import Myo.Init (initialize)
import Myo.Ui.Toggle (myoToggleLayout, myoTogglePane)

plugin' :: Ribosome Env -> Plugin (Ribosome Env)
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

plugin :: FilePath -> Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin tempdir = do
  env <- initialize tempdir
  wrapPlugin $ plugin' env
