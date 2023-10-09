module Myo.Command.Data.RunError where

import Chiasma.Data.CodecError (CodecError)
import Chiasma.Data.RenderError (RenderError)
import Chiasma.Data.TmuxError (TmuxError)
import Chiasma.Data.Views (ViewsError)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Data.Some (Some (Some))
import Exon (exon)
import Log (Severity (Debug, Error, Warn))
import Ribosome (PersistError, Report (Report), Reportable (..), RpcError, rpcError)
import Time (MilliSeconds (MilliSeconds))

import Myo.Command.Data.Command (ident)
import qualified Myo.Command.Data.Command as Cmd (Command (Command))
import Myo.Command.Data.CommandError (CommandError (..))
import Myo.Command.Data.Param (ParamId, ParamTag, paramTagName, paramTagType)
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
  Internal Text
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
  |
  NoParamValue ParamId Text Text
  |
  BadParamValue (Some ParamTag) (Maybe RpcError)
  |
  ParamFunError (Some ParamTag) RpcError
  deriving stock (Show)

instance Reportable RunError where
  toReport (Command e) = toReport e

  toReport (NoRunner task) =
    Report user ["no runner for task:", show task] Warn
    where
      user = "no runner available for command `" <> commandIdText (task ^. #command . #ident) <> "`"

  toReport (Toggle e) =
    toReport e

  toReport (Views e) =
    Report "tmux error" ["RunError.Views:", show e] Error

  toReport (Tmux e) =
    Report "tmux error" ["RunError.Tmux:", show e] Error

  toReport (TmuxCodec e) =
    Report "tmux codec error" ["RunError.TmuxCodec:", show e] Error

  toReport (Internal e) =
    Report [exon|Internal error: #{e}|] ["RunError.Internal:", e] Error

  toReport (IOEmbed e) =
    Report "internal error" ["embedded IO had unexpected error:", e] Debug

  toReport SocketFailure =
    Report "internal error" ["could not create listener socket"] Error

  toReport (InvalidShell command@(Cmd.Command {ident})) =
    Report msg ["RunError.InvalidShell:", show command] Error
    where
      msg = "invalid command for shell: " <> show ident

  toReport (InvalidCmdline err) =
    Report msg [msg] Warn
    where
      msg =
        "invalid command line: " <> err

  toReport (Unsupported runner tpe) =
    Report msg [msg] Warn
    where
      msg =
        "runner `" <> runner <> "` does not support " <> tpe <> " commands"

  toReport (VimTest e) =
    Report "vim-test failed" ["RunError.VimTest:", e] Error

  toReport NoLinesSpecified =
    Report "no lines specified for command" ["RunError.NoLinesSpecified"] Warn

  toReport (SubprocFailed msg out) =
    Report [exon|subprocess failed: #{userErr out}|] ("RunError.SubprocFailed" : out) Warn
    where
      userErr [] = msg
      userErr (e : _) = ": " <> e

  toReport (Render e) =
    Report "tmux error" ["RunError.Render:", show e] Error

  toReport (TreeMod e) =
    Report "tmux error" ["RunError.TreeMod:", show e] Error

  toReport (Proc err) =
    Report "Could not determine tmux process ID" ["RunError.Proc:", show err] Error

  toReport (Persist err) =
    toReport err

  toReport (Rpc err) =
    toReport err

  toReport (ShellDidntStart i (MilliSeconds timeout)) =
    Report [exon|The shell '#{commandIdText i}' didn't start within #{show timeout}ms|] log Error
    where
      log = ["RunError.ShellDidntStart:", show i, show timeout]

  toReport (NoParamValue pid var fun) =
    Report [exon|Neither 'g:#{var}' nor '#{fun}()' exists for command parameter '##{pid}' without default|] log Error
    where
      log = ["RunError.NoParamValue:", show pid, var, fun]

  toReport (BadParamValue (Some ptag) err) =
    Report [exon|The value returned for command parameter '#{name}' is not a #{tpe}|] log Error
    where
      log = ["RunError.BadParamValue:", name] <> foldMap (pure . rpcError) err
      tpe = paramTagType ptag
      name = paramTagName ptag

  toReport (ParamFunError (Some ptag) err) =
    Report [exon|The function for command parameter '#{name}' failed: #{rpcError err}|] log Error
    where
      log = ["RunErroParamFunError:", name, rpcError err]
      name = paramTagName ptag
