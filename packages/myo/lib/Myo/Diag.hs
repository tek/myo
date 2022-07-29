module Myo.Diag where

import qualified Data.Map.Strict as Map
import Exon (exon)
import Prettyprinter (Doc, line, nest, pretty, vsep)
import Ribosome (
  Handler,
  Report (Report),
  ReportContext,
  Reports,
  RpcError,
  Scratch,
  Settings,
  StoredReport (StoredReport),
  reportContext,
  resumeReport,
  storedReports,
  )
import qualified Ribosome.Scratch as Scratch
import Ribosome.Scratch (ScratchOptions (focus, name, syntax))
import Ribosome.Syntax (HiLink (..), Syntax (Syntax), SyntaxItem (..), syntaxMatch)

import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.CommandState as CommandState
import Myo.Command.Data.CommandState (CommandState)
import Myo.Ui.Data.Space (Space)
import qualified Myo.Ui.Data.UiState as UiState
import Myo.Ui.Data.UiState (UiState)

headlineMatch :: SyntaxItem
headlineMatch =
  syntaxMatch "MyoDiagHeadline" "^#\\+ .*"

itemMatch :: SyntaxItem
itemMatch =
  syntaxMatch "MyoDiagItem" "^* .*"

headlineLink :: HiLink
headlineLink =
  HiLink "MyoDiagHeadline" "Title"

itemLink :: HiLink
itemLink =
  HiLink "MyoDiagItem" "Directory"

diagnosticsSyntax :: Syntax
diagnosticsSyntax =
  Syntax items highlights hilinks
  where
    items = [headlineMatch, itemMatch]
    highlights = []
    hilinks = [headlineLink, itemLink]

cmdDiagnostics :: [Command] -> Doc a
cmdDiagnostics cmds =
  "## Commands" <> line <> vsep (pretty <$> cmds)

uiDiagnostics :: [Space] -> Doc a
uiDiagnostics spaces =
  "## Ui" <> line <> (vsep . fmap pretty) spaces

storedError :: StoredReport -> Doc a
storedError (StoredReport (Report _ log _) _) =
  case log of
    [] -> mempty
    (h : t) ->
      nest 2 (vsep (pretty <$> ([exon|* #{h}|] : t)))

tagErrors :: ReportContext -> [StoredReport] -> Doc a
tagErrors t errs =
  pretty [exon|### #{reportContext t}|] <> line <> vsep (storedError <$> errs)

errorDiagnostics :: Map ReportContext [StoredReport] -> Doc a
errorDiagnostics errs | null errs =
  mempty
errorDiagnostics errs =
  "## Reports" <> line <> line <> vsep (uncurry tagErrors <$> Map.toAscList errs)

diagnostics ::
  Members [Settings !! se, AtomicState UiState, AtomicState CommandState, Reports] r =>
  Sem r (Doc a)
diagnostics = do
  cmds <- atomicGets (cmdDiagnostics . CommandState.commands)
  ui <- atomicGets (uiDiagnostics . UiState.spaces)
  errors <- errorDiagnostics <$> storedReports
  pure $ headline <> line <> line <> cmds <> line <> line <> ui <> line <> line <> errors
  where
    headline = "# Diagnostics"

myoDiag ::
  Members [Settings !! se, Scratch !! RpcError, AtomicState UiState, AtomicState CommandState, Reports] r =>
  Handler r ()
myoDiag =
  resumeReport @Scratch do
    content <- diagnostics
    void $ Scratch.show (lines (show content)) options
  where
    options =
      def {
        name = "myo-diagnostics",
        focus = True,
        syntax = [diagnosticsSyntax]
      }
