{-# LANGUAGE TemplateHaskell #-}

module Myo.Data.Env where

import Chiasma.Data.Ident (Ident(Str))
import Control.Concurrent.STM.TMChan (TMChan)
import Data.DeepLenses (deepLenses)
import Data.Default (Default(def))
import Ribosome.Control.Monad.Ribo (ConcNvimS, Ribo, RiboE)
import Ribosome.Data.Errors (Errors)

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import Myo.Ui.Data.PaneOutput (PaneOutput)
import Myo.Ui.Data.UiState (UiState)

type Myo a = Ribo Env (ConcNvimS Env) a
type MyoE e m a = RiboE Env e m a

type CanRun = RunTask -> Bool
type RunF = RunTask -> MyoE RunError (ConcNvimS Env) ()

data Runner =
  Runner Ident CanRun RunF

data Env =
  Env {
    _command :: CommandState,
    _ui :: UiState,
    _errors :: Errors,
    _runners :: [Runner],
    _instanceIdent :: Ident,
    _tempDir :: FilePath,
    _watcherChan :: Maybe (TMChan PaneOutput)
  }

deepLenses ''Env

instance Default Env where
  def = Env def def def def (Str "myo") def def
