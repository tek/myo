module Myo.Command.Output where

import Control.Monad.DeepState (MonadDeepState, getsL, setL)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as Text (unpack)
import Data.Traversable (mapAccumL)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.ScratchOptions (defaultScratchOptions, scratchSyntax)
import Ribosome.Data.Syntax (Syntax)
import Ribosome.Scratch (showInScratchOrCreate)

import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (parseReports, reportScratch)
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import qualified Myo.Output.Data.ParsedOutput as ParsedOutput (ParsedOutput(_syntax))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))

renderReport ::
  MonadIO m =>
  NvimE e m =>
  MonadDeepState s CommandState m =>
  ParseReport ->
  [Syntax] ->
  m ()
renderReport (ParseReport _ lines') syntax = do
  oldScratch <- getsL @CommandState CommandState.reportScratch
  scratch <- showInScratchOrCreate oldScratch (render <$> lines')  options
  setL @CommandState CommandState.reportScratch (Just scratch)
  where
    render (ReportLine _ text) =
      Text.unpack text
    options = scratchSyntax syntax (defaultScratchOptions "myo-report")

renderParseResult ::
  MonadIO m =>
  NvimE e m =>
  MonadDeepState s CommandState m =>
  [ParsedOutput] ->
  m ()
renderParseResult output = do
  setL @CommandState CommandState.parseReports (Just report)
  renderReport report syntax
  where
    report = mconcat reports
    syntax = ParsedOutput._syntax <$> output
    (_, reports) = mapAccumL format 0 output
    format offset (ParsedOutput _ cons) =
      (offset + length events, report')
      where
        report'@(ParseReport events _) = cons offset
