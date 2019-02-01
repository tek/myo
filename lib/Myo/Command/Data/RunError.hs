module Myo.Command.Data.RunError(
  RunError(..),
) where

import Chiasma.Data.Ident (identString)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (ViewsError)
import Ribosome.Error.Report (ReportError(..), ErrorReport(ErrorReport))
import System.Log (Priority(ERROR, NOTICE))

import qualified Myo.Command.Data.Command as Cmd (Command(Command))
import Myo.Command.Data.CommandError (CommandError(..))
import Myo.Command.Data.RunTask (RunTask(RunTask))
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Error (viewsErrorReport, tmuxErrorReport)

data RunError =
  RunError String
  |
  Command CommandError
  |
  NoRunner RunTask
  |
  Toggle ToggleError
  |
  Views ViewsError
  |
  Tmux TmuxError
  deriving (Eq, Show)

instance ReportError RunError where
  errorReport (RunError e) =
    ErrorReport e ["running command failed:", e] ERROR
  errorReport (Command e) = errorReport e
  errorReport (NoRunner task@(RunTask (Cmd.Command _ ident _ _) _ _)) =
    ErrorReport user ["no runner for task:", show task] NOTICE
    where
      user = "no runner available for command `" ++ identString ident ++ "`"
  errorReport (Toggle e) =
    errorReport e
  errorReport (Views e) = viewsErrorReport e
  errorReport (Tmux e) = tmuxErrorReport e
