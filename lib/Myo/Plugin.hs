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
import Ribosome.Control.Monad.Ribo (ConcNvimS)
import Ribosome.Control.Ribosome (Ribosome)
import Ribosome.Error.Report (reportError)
import Ribosome.Plugin (cmd, nvimPlugin, rpcHandler, rpcHandlerDef, sync)

import Myo.Command.Add (myoAddShellCommand, myoAddSystemCommand)
import Myo.Command.Run (myoRun)
import Myo.Data.Env (Env, MyoE)
import Myo.Data.Error (Error)
import Myo.Diag (myoDiag)
import Myo.Init (initialize, myoPoll)
import Myo.Ui.Toggle (myoToggleLayout, myoTogglePane)

handleError :: Error -> MyoE Error (ConcNvimS Env) ()
handleError =
  reportError "myo"

plugin' :: Ribosome Env -> Plugin (Ribosome Env)
plugin' env =
  nvimPlugin env funcs handleError
  where
    funcs = [
      $(rpcHandler (cmd []) 'myoDiag),
      $(rpcHandler sync 'myoPoll),
      $(rpcHandlerDef 'myoAddSystemCommand),
      $(rpcHandlerDef 'myoAddShellCommand),
      $(rpcHandlerDef 'myoTogglePane),
      $(rpcHandlerDef 'myoToggleLayout),
      $(rpcHandler (cmd []) 'myoRun)
      ]

plugin :: FilePath -> Neovim (StartupConfig NeovimConfig) NeovimPlugin
plugin tempdir = do
  env <- initialize tempdir
  wrapPlugin $ plugin' env
