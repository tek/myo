{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.QuitSpec(
  htf_thisModulesTests,
) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Ribosome.Plugin.Mapping (executeMapping)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Ribosome.Test.Ui (windowCountIs)
import Test.Framework

import Myo.Command.Output (renderParseResult)
import Myo.Data.Env (MyoN)
import Myo.Init (initialize'')
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine)
import Myo.Output.Lang.Haskell.Report (HaskellMessage(FoundReq1, NoMethod), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Myo.Plugin (mappingOutputQuit)

events :: Vector OutputEvent
events =
  Vector.fromList [OutputEvent Nothing 0, OutputEvent Nothing 1]

loc :: Location
loc =
  Location "/path/to/File.hs" 10 Nothing

reportLines :: Vector ReportLine
reportLines =
  formatReportLine 0 loc (FoundReq1 "TypeA" "TypeB") <> formatReportLine 0 loc (NoMethod "fmap")

parsedOutput :: ParsedOutput
parsedOutput =
  ParsedOutput haskellSyntax (const $ ParseReport events reportLines)

outputQuitSpec :: MyoN ()
outputQuitSpec = do
  initialize''
  renderParseResult (Ident.Str "test") [parsedOutput]
  windowCountIs 2
  executeMapping mappingOutputQuit
  windowCountIs 1

test_outputQuit :: IO ()
test_outputQuit =
  tmuxGuiSpecDef outputQuitSpec
