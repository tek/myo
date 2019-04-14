module Myo.Ui.Error where

import Chiasma.Data.Cmd (Cmds(Cmds))
import Chiasma.Data.Ident (Ident, identText)
import Chiasma.Data.RenderError (RenderError)
import qualified Chiasma.Data.RenderError as RenderError (RenderError(..))
import Chiasma.Data.TmuxError (TmuxError(..))
import Chiasma.Data.Views (ViewsError(..))
import Chiasma.Ui.Data.TreeModError (TreeModError(..))
import Chiasma.Ui.Data.View (View(View))
import Ribosome.Data.ErrorReport (ErrorReport(ErrorReport))
import System.Log (Priority(ERROR, DEBUG, NOTICE))

invalidOutput :: Text
invalidOutput = "invalid output from tmux process"

tmuxErrorReport :: TmuxError -> ErrorReport
tmuxErrorReport (ProcessFailed (Cmds cmds) reason) =
  ErrorReport "fatal error in tmux process" log' ERROR
  where
    log' = ["tmux process failed:", reason, "commands:"] <> (show <$> cmds)
tmuxErrorReport (OutputParsingFailed (Cmds cmds) output parseError) =
  ErrorReport invalidOutput (["tmux output parsing failed:"] <> (show <$> cmds) <> ["output:"] <>
  output <> ["parse error:", show parseError]) ERROR
tmuxErrorReport (NoOutput (Cmds cmds)) =
  ErrorReport invalidOutput ("no output from tmux process:" : (show <$> cmds)) ERROR
tmuxErrorReport (DecodingFailed (Cmds cmds) output decodeError) =
  ErrorReport invalidOutput ("failed to decode tmux process output:" : (show <$> cmds) <> ["output:", output,
    "decoding error:", show decodeError]) ERROR
tmuxErrorReport (InvalidOutput reason cmd) =
  ErrorReport invalidOutput ["invalid output from tmux process:", toText reason, toText cmd] ERROR
tmuxErrorReport (CommandFailed _ err) =
  ErrorReport invalidOutput ("tmux command failed:" : err) ERROR

noSuchView :: Text -> Ident -> ErrorReport
noSuchView desc ident =
  ErrorReport msg [msg] NOTICE
  where
    msg = "no tmux " <> desc <> " with ident `" <> identText ident <> "`"

noId :: Text -> Ident -> ErrorReport
noId desc ident =
  ErrorReport msg [msg] ERROR
  where
    msg = "tmux " <> desc <> " with ident `" <> identText ident <> "`" <> " has no id"

viewsErrorReport :: ViewsError -> ErrorReport
viewsErrorReport (NoSuchSession ident) =
  noSuchView "session" ident
viewsErrorReport (NoSuchWindow ident) =
  noSuchView "window" ident
viewsErrorReport (NoSuchPane ident) =
  noSuchView "pane" ident
viewsErrorReport (NoPaneId ident) =
  noId "pane" ident

renderErrorReport :: RenderError -> ErrorReport
renderErrorReport (RenderError.NoPrincipal ident) =
  ErrorReport "internal render error: no view in layout" ["no principal in " <> identText ident] ERROR
renderErrorReport (RenderError.Views err) =
  viewsErrorReport err
renderErrorReport (RenderError.Pack message) =
  ErrorReport ("error packing a tmux layout: " <> toText message) ["tmux pack error:", toText message] ERROR
renderErrorReport (RenderError.Fatal tmuxError) =
  tmuxErrorReport tmuxError

viewExists :: Text -> View a -> ErrorReport
viewExists desc (View ident _ _ _) =
  ErrorReport msg [msg] DEBUG
  where
    msg = "a " <> desc <> " with ident `" <> identText ident <> "` already exists"

viewMissing :: Text -> Ident -> ErrorReport
viewMissing desc ident =
  ErrorReport msg [msg] DEBUG
  where
    msg = "no " <> desc <> " with ident `" <> identText ident <> "`"

ambiguousView :: Text -> Ident -> Int -> ErrorReport
ambiguousView desc ident num =
  ErrorReport msg [logMsg] ERROR
  where
    msg = "there are " <> show num <> " " <> desc <> "s with ident `" <> identText ident <> "`"
    logMsg = "ambiguous " <> desc <> ": " <> identText ident <> "(" <> show num <> ")"

treeModErrorReport :: TreeModError -> ErrorReport
treeModErrorReport (PaneExists pane) =
  viewExists "pane" pane
treeModErrorReport (LayoutExists layout) =
  viewExists "layout" layout
treeModErrorReport (PaneMissing pane) =
  viewMissing "pane" pane
treeModErrorReport (LayoutMissing layout) =
  viewMissing "layout" layout
treeModErrorReport (AmbiguousPane pane num) =
  ambiguousView "pane" pane num
treeModErrorReport (AmbiguousLayout layout num) =
  ambiguousView "layout" layout num
treeModErrorReport NoTrees =
  ErrorReport msg [msg] DEBUG
  where
    msg = "no UI layouts have been created"
