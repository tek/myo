module Myo.Diag where

import Control.Monad.DeepState (MonadDeepState, getsL)
import Data.Functor (void)
import Data.Text.Prettyprint.Doc (line, pretty, (<>))
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
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

cmdDiagnostics :: [Command] -> [Text]
cmdDiagnostics cmds =
  lines =<< (show . (line <>) . pretty <$> cmds)

errorDiagnostics :: Errors -> [Text]
errorDiagnostics =
  lines . show . (line <>) . pretty

diagnosticsData ::
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  m [Text]
diagnosticsData = do
  cmds <- getsL @CommandState CommandState.commands
  errors <- getsL @Env Env.errors
  return $ ["# Diagnostics", "", "## Commands"] <> cmdDiagnostics cmds <> ["", "## Errors"] <> errorDiagnostics errors

myoDiag ::
  MonadRibo m =>
  MonadDeepState s Env m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  NvimE e m =>
  m ()
myoDiag = do
  content <- diagnosticsData
  void $ showInScratch content options
  where
    options = scratchFocus $ scratchSyntax [diagnosticsSyntax] $ defaultScratchOptions "myo-diagnostics"
