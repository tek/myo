{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Myo.Data.Env where

import Chiasma.Data.Ident (Ident(Str))
import Data.DeepLenses (deepLenses)
import Data.Default (Default(def))
import Path (Abs, Dir, Path, absdir)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Orphans ()
import qualified Text.Show (Show(show))

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import Myo.Data.Error (Error)
import Myo.Ui.Data.UiState (UiState)

type Myo a = Ribo Env Error a

type CanRun = RunTask -> Bool
type RunF = RunTask -> IO (Either RunError ())

data Runner =
  Runner {
    runnerIdent :: Ident,
    runnerEligible :: CanRun,
    runnerRun :: RunF
  }

class Runner1 a where

instance Text.Show.Show Runner where
  show (Runner ident _ _) =
    "Runner(" <> show ident <> ")"

data Env =
  Env {
    _command :: CommandState,
    _ui :: UiState,
    _runners :: [Runner],
    _instanceIdent :: Ident,
    _tempDir :: Path Abs Dir
  }

deepLenses ''Env

instance Default Env where
  def = Env def def def (Str "myo") [absdir|/tmp/myo|]

instance Text.Show.Show Env where
  show (Env cmds ui' _ ii td) = "Env" ++ show (cmds, ui', ii, td)
