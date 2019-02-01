module Myo.Ui.Error(
  tmuxErrorReport,
  renderErrorReport,
  treeModErrorReport,
  viewsErrorReport,
) where

import Chiasma.Data.Cmd (Cmds(Cmds))
import Chiasma.Data.Ident (Ident, identString)
import Chiasma.Data.RenderError (RenderError)
import qualified Chiasma.Data.RenderError as RenderError (RenderError(..))
import Chiasma.Data.TmuxThunk (TmuxError(..))
import Chiasma.Data.Views (ViewsError(..))
import Chiasma.Ui.Data.TreeModError (TreeModError(..))
import Chiasma.Ui.Data.View (View(View))
import Ribosome.Error.Report (ErrorReport(ErrorReport))
import System.Log (Priority(ERROR, DEBUG))

invalidOutput :: String
invalidOutput = "invalid output from tmux process"

tmuxErrorReport :: TmuxError -> ErrorReport
tmuxErrorReport (ProcessFailed (Cmds cmds) reason) =
  ErrorReport "fatal error in tmux process" (["tmux process failed:", reason, "commands:"] ++ (show <$> cmds)) ERROR
tmuxErrorReport (OutputParsingFailed (Cmds cmds) output parseError) =
  ErrorReport invalidOutput (["tmux output parsing failed:"] ++ (show <$> cmds) ++ ["output:"] ++ output ++
    ["parse error:"] ++ [show parseError]) ERROR
tmuxErrorReport (NoOutput (Cmds cmds)) =
  ErrorReport invalidOutput ("no output from tmux process:" : (show <$> cmds)) ERROR
tmuxErrorReport (DecodingFailed (Cmds cmds) output decodeError) =
  ErrorReport invalidOutput ("failed to decode tmux process output:" : (show <$> cmds) ++ ["output:", output,
    "decoding error:", show decodeError]) ERROR
tmuxErrorReport (InvalidOutput reason cmd) =
  ErrorReport invalidOutput ["invalid output from tmux process:", reason, cmd] ERROR

noSuchView :: String -> Ident -> ErrorReport
noSuchView desc ident =
  ErrorReport msg [msg] ERROR
  where
    msg = "no tmux " ++ desc ++ " with ident `" ++ identString ident ++ "`"

viewsErrorReport :: ViewsError -> ErrorReport
viewsErrorReport (NoSuchSession ident) =
  noSuchView "session" ident
viewsErrorReport (NoSuchWindow ident) =
  noSuchView "window" ident
viewsErrorReport (NoSuchPane ident) =
  noSuchView "pane" ident

renderErrorReport :: RenderError -> ErrorReport
renderErrorReport (RenderError.RenderError message) =
  ErrorReport message ["tmux render error:", message] ERROR
renderErrorReport (RenderError.Views err) =
  viewsErrorReport err
renderErrorReport (RenderError.Pack message) =
  ErrorReport ("error packing a tmux layout: " ++ message) ["tmux pack error:", message] ERROR
renderErrorReport (RenderError.Fatal tmuxError) =
  tmuxErrorReport tmuxError

viewExists :: String -> View a -> ErrorReport
viewExists desc (View ident _ _ _) =
  ErrorReport msg [msg] DEBUG
  where
    msg = "a " ++ desc ++ " with ident `" ++ identString ident ++ "` already exists"

viewMissing :: String -> Ident -> ErrorReport
viewMissing desc ident =
  ErrorReport msg [msg] DEBUG
  where
    msg = "no " ++ desc ++ " with ident `" ++ identString ident ++ "`"

ambiguousView :: String -> Ident -> Int -> ErrorReport
ambiguousView desc ident num =
  ErrorReport msg [logMsg] ERROR
  where
    msg = "there are " ++ show num ++ " " ++ desc ++ "s with ident `" ++ identString ident ++ "`"
    logMsg = "ambiguous " ++ desc ++ ": " ++ identString ident ++ "(" ++ show num ++ ")"

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
