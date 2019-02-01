{-# LANGUAGE TemplateHaskell #-}

module Myo.Data.Env(
  Env(..),
  _command,
  _ui,
  _errors,
  _runners,
  _tempDir,
  Myo,
  Ribo,
  MyoE,
  RiboE,
  Runner(..),
  RunF,
  CanRun,
) where

import Chiasma.Data.Ident (Ident(Str))
import Control.Lens (makeClassy_)
import Data.Default (Default(def))
import Ribosome.Control.Monad.RiboE (RiboE, Ribo)
import Ribosome.Data.Errors (Errors)

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.Pid (Pid)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import Myo.Ui.Data.UiState (UiState)

type Myo a = Ribo Env a
type MyoE a = RiboE Env a

type CanRun = RunTask -> Bool
type RunF = RunTask -> MyoE RunError (Maybe Pid)

data Runner =
  Runner Ident CanRun RunF

data Env =
  Env {
    command :: CommandState,
    ui :: UiState,
    errors :: Errors,
    runners :: [Runner],
    instanceIdent :: Ident,
    tempDir :: FilePath
  }

makeClassy_ ''Env

instance Default Env where
  def = Env def def def def (Str "myo") def
