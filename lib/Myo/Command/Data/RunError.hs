{-# LANGUAGE TemplateHaskell #-}

module Myo.Command.Data.RunError where

import Chiasma.Data.Ident (identString)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (ViewsError)
import Data.DeepPrisms (deepPrisms)
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import Ribosome.Error.Report.Class (ReportError(..))
import Ribosome.Nvim.Api.RpcCall (RpcError)
import System.Log (Priority(NOTICE, DEBUG))

import qualified Myo.Command.Data.Command as Cmd (Command(Command))
import Myo.Command.Data.CommandError (CommandError(..))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Error (tmuxErrorReport, viewsErrorReport)

data RunError =
  Command CommandError
  |
  NoRunner RunTask
  |
  Toggle ToggleError
  |
  Views ViewsError
  |
  Tmux TmuxError
  |
  Rpc RpcError
  |
  IOEmbed String
  deriving Show

deepPrisms ''RunError

instance ReportError RunError where
  errorReport (Command e) = errorReport e
  errorReport (NoRunner task@(RunTask (Cmd.Command _ ident _ _ _) _ _)) =
    ErrorReport user ["no runner for task:", show task] NOTICE
    where
      user = "no runner available for command `" ++ identString ident ++ "`"
  errorReport (Toggle e) =
    errorReport e
  errorReport (Views e) = viewsErrorReport e
  errorReport (Tmux e) = tmuxErrorReport e
  errorReport (Rpc e) = errorReport e
  errorReport (IOEmbed e) = ErrorReport "internal error" ["embedded IO had unexpected error:", e] DEBUG
