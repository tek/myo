{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Myo.Data.Env where

import Chiasma.Data.Ident (Ident(Str))
import Data.DeepLenses (deepLenses)
import Data.Default (Default(def))
import Data.Hourglass (ElapsedP(ElapsedP))
import Path (Abs, Dir, Path, absdir)
import Ribosome.Control.Monad.Ribo (Ribo)
import Ribosome.Orphans ()
import Ribosome.System.Time (secondsP)
import qualified Text.Show (Show(show))

import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.Execution (ExecutionState)
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import Myo.Data.Error (Error)
import Myo.Ui.Data.UiState (UiState)

type Myo a = Ribo Env Error a

type CanRun = RunTask -> Bool
type RunF = RunTask -> IO (Either RunError ())
type CheckPending = RunTask -> IO (Either RunError (IO ExecutionState))

data Runner =
  Runner {
    runnerIdent :: Ident,
    runnerEligible :: CanRun,
    runnerRun :: RunF,
    runnerCheckPending :: CheckPending
  }

class Runner1 a where

instance Text.Show.Show Runner where
  show (Runner ident _ _ _) =
    "Runner(" <> show ident <> ")"

data Env =
  Env {
    _command :: CommandState,
    _ui :: UiState,
    _runners :: [Runner],
    _instanceIdent :: Ident,
    _tempDir :: Path Abs Dir,
    _lastSave :: ElapsedP
  }

deepLenses ''Env

instance Default Env where
  def = Env def def def (Str "myo") [absdir|/tmp/myo|] (secondsP 0.0)

instance Text.Show.Show Env where
  show (Env cmds ui' _ ii td ls) = "Env" ++ show (cmds, ui', ii, td, ls)
