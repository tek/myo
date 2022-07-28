module Myo.Command.Data.RunError where

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.Ident (identText, identify)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (ViewsError)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Exon (exon)
import Log (Severity (Debug, Error, Warn))
import Ribosome (ErrorMessage (ErrorMessage), PersistError, RpcError, ToErrorMessage (..))
import Time (MilliSeconds (MilliSeconds))

import Myo.Command.Data.Command (ident)
import qualified Myo.Command.Data.Command as Cmd (Command (Command))
import Myo.Command.Data.CommandError (CommandError (..))
import Myo.Command.Data.RunTask (RunTask)
import Myo.Data.CommandId (CommandId, commandIdText)
import Myo.Ui.Data.ToggleError (ToggleError)

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
  TmuxCodec CodecError
  |
  IOEmbed Text
  |
  SocketFailure
  |
  InvalidShell Cmd.Command
  |
  InvalidCmdline Text
  |
  Unsupported Text Text
  |
  VimTest Text
  |
  NoLinesSpecified
  |
  SubprocFailed Text [Text]
  |
  Render RenderError
  |
  TreeMod TreeModError
  |
  Proc Text
  |
  Persist PersistError
  |
  Rpc RpcError
  |
  ShellDidntStart CommandId MilliSeconds
  deriving stock (Show)

instance ToErrorMessage RunError where
  toErrorMessage (Command e) = toErrorMessage e
  toErrorMessage (NoRunner task) =
    ErrorMessage user ["no runner for task:", show task] Warn
    where
      user = "no runner available for command `" <> identText (identify task) <> "`"
  toErrorMessage (Toggle e) =
    toErrorMessage e
  toErrorMessage (Views e) =
      ErrorMessage "tmux error" ["RunError.Views:", show e] Error
  toErrorMessage (Tmux e) =
      ErrorMessage "tmux error" ["RunError.Tmux:", show e] Error
  toErrorMessage (TmuxCodec e) =
      ErrorMessage "tmux codec error" ["RunError.TmuxCodec:", show e] Error
  toErrorMessage (IOEmbed e) =
    ErrorMessage "internal error" ["embedded IO had unexpected error:", e] Debug
  toErrorMessage SocketFailure =
    ErrorMessage "internal error" ["could not create listener socket"] Error
  toErrorMessage (InvalidShell command@(Cmd.Command {ident})) =
    ErrorMessage msg ["RunError.InvalidShell:", show command] Error
    where
      msg = "invalid command for shell: " <> show ident
  toErrorMessage (InvalidCmdline err) =
    ErrorMessage msg [msg] Warn
    where
      msg =
        "invalid command line: " <> err
  toErrorMessage (Unsupported runner tpe) =
    ErrorMessage msg [msg] Warn
    where
      msg =
        "runner `" <> runner <> "` does not support " <> tpe <> " commands"
  toErrorMessage (VimTest e) =
    ErrorMessage "vim-test failed" ["RunError.VimTest:", e] Error
  toErrorMessage NoLinesSpecified =
    ErrorMessage "no lines specified for command" ["RunError.NoLinesSpecified"] Warn
  toErrorMessage (SubprocFailed msg out) =
    ErrorMessage [exon|subprocess failed: #{userErr out}|] ("RunError.SubprocFailed" : out) Warn
    where
      userErr [] = msg
      userErr (e : _) = ": " <> e
  toErrorMessage (Render e) =
    ErrorMessage "tmux error" ["RunError.Render:", show e] Error
  toErrorMessage (TreeMod e) =
    ErrorMessage "tmux error" ["RunError.TreeMod:", show e] Error
  toErrorMessage (Proc err) =
    ErrorMessage "Could not determine tmux process ID" ["RunError.Proc:", show err] Error
  toErrorMessage (Persist err) =
    toErrorMessage err
  toErrorMessage (Rpc err) =
    toErrorMessage err
  toErrorMessage (ShellDidntStart i (MilliSeconds timeout)) =
    ErrorMessage [exon|The shell `#{commandIdText i}` didn't start within #{show timeout}ms|] log Error
    where
      log =
        ["RunError.ShellDidntStart:", show i, show timeout]
