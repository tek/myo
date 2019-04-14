module Myo.Command.Output where

import Chiasma.Data.Ident (Ident)
import Control.Monad (void, when)
import Control.Monad.DeepError (hoistMaybe)
import Control.Monad.DeepState (MonadDeepState, setL)
import Control.Monad.IO.Class (MonadIO)
import Data.Traversable (mapAccumL)
import Ribosome.Api.Echo (echom)
import Ribosome.Control.Monad.Ribo (MonadRibo, NvimE)
import Ribosome.Data.Mapping (Mapping(Mapping), MappingIdent(MappingIdent))
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchMappings, scratchSyntax)
import Ribosome.Data.Syntax (Syntax)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Scratch (killScratch, scratchPreviousWindow, showInScratch)

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (parseReport, parseResult)
import Myo.Output.Data.OutputError (OutputError(NoEvents, NotParsed))
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(Internal))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport), noEventsInReport)
import Myo.Output.Data.ParseResult (ParseResult(ParseResult))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import qualified Myo.Output.Data.ParsedOutput as ParsedOutput (ParsedOutput(_syntax))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))
import Myo.Output.ParseReport (selectCurrentLineEventFrom)

scratchName :: Text
scratchName =
  "myo-report"

mappings :: [Mapping]
mappings =
  [
    Mapping (MappingIdent "output-quit") "q" "n" False True,
    Mapping (MappingIdent "output-select") "<cr>" "n" False True
    ]

renderReport ::
  MonadRibo m =>
  MonadIO m =>
  NvimE e m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  ParseReport ->
  [Syntax] ->
  m ()
renderReport (ParseReport _ lines') syntax =
  void $ showInScratch (render <$> lines') options
  where
    render (ReportLine _ text) =
      text
    options =
      scratchMappings mappings . scratchSyntax syntax . defaultScratchOptions $ scratchName

renderParseResult ::
  MonadRibo m =>
  MonadIO m =>
  NvimE e m =>
  MonadDeepState s CommandState m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e OutputError m =>
  Ident ->
  [ParsedOutput] ->
  m ()
renderParseResult ident output = do
  when (noEventsInReport report) (throwHoist (NoEvents ident))
  setL @CommandState CommandState.parseReport (Just report)
  renderReport report syntax
  where
    report = mconcat reports
    syntax = ParsedOutput._syntax <$> output
    (_, reports) = mapAccumL format 0 output
    format offset (ParsedOutput _ cons) =
      (offset + length events, report')
      where
        report'@(ParseReport events _) = cons offset

outputQuit ::
  MonadRibo m =>
  NvimE e m =>
  m ()
outputQuit =
  killScratch scratchName

outputSelect ::
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepState s CommandState m =>
  MonadRibo m =>
  NvimE e m =>
  m ()
outputSelect = do
  window <- hoistMaybe (OutputError.Internal "no scratch") =<< scratchPreviousWindow scratchName
  (ParseResult ident _) <- hoistMaybe NotParsed =<< getsL @CommandState CommandState.parseResult
  report <- hoistMaybe (NoEvents ident) =<< getsL @CommandState CommandState.parseReport
  selectCurrentLineEventFrom report window
