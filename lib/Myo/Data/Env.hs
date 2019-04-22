{-# LANGUAGE TemplateHaskell #-}

module Myo.Data.Env where

import Chiasma.Data.Ident (Ident(Str))
import Data.DeepLenses (deepLenses)
import Data.Default (Default(def))
import Path (Abs, Dir, Path, absdir)
import Ribosome.Control.Monad.Ribo (ConcNvimS, Ribo, RiboE)
import Ribosome.Data.Errors (Errors)
import Ribosome.Orphans ()
import qualified Text.Show (Show(show))

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import Myo.Data.Error (Error)
import Myo.Ui.Data.UiState (UiState)

type EnvN = ConcNvimS Env
type Myo a = Ribo Env EnvN a
type MyoE e m a = RiboE Env e m a
type MyoN = RiboE Env Error EnvN

type CanRun = RunTask -> Bool
type RunF = RunTask -> IO (Either RunError ())

data Runner =
  Runner Ident CanRun RunF

data Env =
  Env {
    _command :: CommandState,
    _ui :: UiState,
    _errors :: Errors,
    _runners :: [Runner],
    _instanceIdent :: Ident,
    _tempDir :: Path Abs Dir
  }

deepLenses ''Env

instance Default Env where
  def = Env def def def def (Str "myo") [absdir|/tmp/myo|]

instance Text.Show.Show Env where
  show (Env cmds ui errs _ ii td) = "Env" ++ show (cmds, ui, errs, ii, td)
