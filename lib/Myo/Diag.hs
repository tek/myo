module Myo.Diag where

import Control.Monad.DeepState (MonadDeepState, getsL)
import Data.Functor (void)
import Data.Text.Prettyprint.Doc (line, pretty, (<>))
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.Errors (Errors)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSyntax)
import Ribosome.Data.Syntax (
  HiLink(..),
  Syntax(Syntax),
  SyntaxItem(..),
  syntaxMatch,
  )
import Ribosome.Scratch (showInScratch)

import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands)
import Myo.Data.Env (Env)
import qualified Myo.Data.Env as Env (errors)

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

cmdDiagnostics :: [Command] -> [String]
cmdDiagnostics cmds =
  lines =<< (show . (line <>) . pretty <$> cmds)

errorDiagnostics :: Errors -> [String]
errorDiagnostics =
  lines . show . (line <>) . pretty

diagnosticsData ::
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  m [String]
diagnosticsData = do
  cmds <- getsL @CommandState CommandState.commands
  errors <- getsL @Env Env.errors
  return $ ["# Diagnostics", "", "## Commands"] ++ cmdDiagnostics cmds ++ ["", "## Errors"] ++ errorDiagnostics errors

myoDiag ::
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  NvimE e m =>
  m ()
myoDiag = do
  content <- diagnosticsData
  void $ showInScratch content (scratchSyntax [diagnosticsSyntax] $ defaultScratchOptions "myo-diagnostics")
