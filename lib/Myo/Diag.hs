module Myo.Diag where

import Data.Text.Prettyprint.Doc (Doc, line, pretty, vsep)
import Ribosome.Control.Monad.Ribo (inspectErrors)
import Ribosome.Data.Errors (Errors)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchFocus, scratchSyntax)
import Ribosome.Data.Syntax (
  HiLink(..),
  Syntax(Syntax),
  SyntaxItem(..),
  syntaxMatch,
  )
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (showInScratch)

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands)
import Myo.Data.Env (Env)
import Myo.Ui.Data.Space (Space)
import Myo.Ui.Data.UiState (UiState)
import qualified Myo.Ui.Data.UiState as UiState (spaces)

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

errorDiagnostics :: Errors -> Doc a
errorDiagnostics errs =
  "## Errors" <> line <> pretty errs

diagnosticsData ::
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s UiState m =>
  m (Doc a)
diagnosticsData = do
  cmds <- getsL @CommandState CommandState.commands cmdDiagnostics
  ui <- getsL @UiState UiState.spaces uiDiagnostics
  errors <- inspectErrors errorDiagnostics
  return $ headline <> line <> line <> cmds <> line <> line <> ui <> line <> line <> errors
  where
    headline = "# Diagnostics"

myoDiag ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MonadDeepState s UiState m =>
  MonadDeepError e DecodeError m =>
  m ()
myoDiag = do
  content <- diagnosticsData
  void $ showInScratch (lines . show $ content) options
  where
    options = scratchFocus $ scratchSyntax [diagnosticsSyntax] $ defaultScratchOptions "myo-diagnostics"
