{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.HaskellRenderSpec(
  htf_thisModulesTests,
) where

import Ribosome.Data.Time (sleep)
import Test.Framework

import Config (vars)
import Myo.Command.Output (renderParseResult)
import Myo.Data.Env (MyoN)
import Myo.Init (initialize'')
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))
import Myo.Output.Lang.Haskell.Report (HaskellMessage(..), formatReportLine)
import Myo.Test.Unit (tmuxGuiSpec)
import Myo.Tmux.IO (runTmux)
import Ribosome.Api.Buffer (currentBufferContent)
import Test (screenshot)

events :: [OutputEvent]
events =
  [OutputEvent Nothing 0, OutputEvent Nothing 1]

loc :: Location
loc =
  Location "/path/to/File.hs" 10 Nothing

reportLines :: [ReportLine]
reportLines =
  formatReportLine 0 loc (FoundReq1 "TypeA" "TypeB") ++ formatReportLine 0 loc (NoMethod "fmap")

parsedOutput :: ParsedOutput
parsedOutput =
  ParsedOutput (\ x -> ParseReport events reportLines)

target :: [String]
target = [
  "/path/to/File.hs \57505 11",
  "type mismatch",
  "TypeA",
  "TypeB",
  "",
  "/path/to/File.hs \57505 11",
  "method not implemented: fmap",
  ""
  ]

haskellRenderSpec :: MyoN ()
haskellRenderSpec = do
  initialize''
  renderParseResult [parsedOutput]
  content <- currentBufferContent
  gassertEqual target content
  screenshot "render-haskell-parse-result" False 0

test_haskellRender :: IO ()
test_haskellRender =
  vars >>= tmuxGuiSpec haskellRenderSpec
