{-# LANGUAGE TemplateHaskell #-}

module Myo.Plugin where

import Neovim (
  Neovim,
  NeovimConfig,
  NeovimPlugin,
  Plugin(..),
  StartupConfig,
  wrapPlugin,
  )
import Neovim.Plugin.Classes (Synchronous(Sync))
import Ribosome.Control.Monad.Ribo (ConcNvimS)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Plugin (RpcHandlerConfig(RpcHandlerConfig), nvimPlugin, rpcHandler, rpcHandlerDef)

import Myo.Command.Run (myoRun)
import Myo.Data.Env (Env, MyoE)
import Myo.Data.Error (Error)
import Myo.Diag (myoDiag)
import Myo.Init (initialize, myoPoll)
-- import Myo.Ui.Toggle (myoToggleLayout, myoTogglePane)

handleError :: Error -> MyoE Error (ConcNvimS Env) ()
handleError =
  undefined

plugin' :: Ribosome Env -> Plugin (Ribosome Env)
plugin' env =
  nvimPlugin env funcs handleError
  where
    funcs = [
      $(rpcHandlerDef 'myoDiag),
      $(rpcHandler (RpcHandlerConfig Sync Nothing) 'myoPoll),
      -- $(rpcHandlerDef 'myoTogglePane),
      -- $(rpcHandlerDef 'myoToggleLayout),
      $(rpcHandlerDef 'myoRun)
      ]

plugin :: FilePath -> Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin tempdir = do
  env <- initialize tempdir
  wrapPlugin $ plugin' env
