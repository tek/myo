{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.QuitSpec (htf_thisModulesTests) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, zipWith)
import Ribosome.Plugin.Mapping (executeMapping)
import Ribosome.Test.Ui (windowCountIs)
import Test.Framework

import Config (outputAutoJump, outputSelectFirst, svar)
import Myo.Command.Output (renderParseResult)
import Myo.Data.Env (Myo)
import Myo.Init (initialize'')
import qualified Myo.Output.Data.EventIndex as EventIndex (Relative)
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (LangOutputEvent(LangOutputEvent), OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine)
import Myo.Output.Lang.Haskell.Report (HaskellMessage(FoundReq1, NoMethod), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Plugin (mappingOutputQuit)
import Unit (tmuxSpec)

loc :: Location
loc =
  Location "/path/to/File.hs" 10 Nothing

events :: Vector OutputEvent
events =
  Vector.fromList [OutputEvent (Just loc) 0, OutputEvent (Just loc) 1]

messages :: Vector HaskellMessage
messages =
  Vector.fromList [FoundReq1 "TypeA" "TypeB", NoMethod "fmap"]

parsedOutput :: ParsedOutput
parsedOutput =
  ParsedOutput haskellSyntax (parsedOutputCons formatReportLine (Vector.zipWith LangOutputEvent events messages))

outputQuitSpec :: Myo ()
outputQuitSpec = do
  initialize''
  renderParseResult (Ident.Str "test") [parsedOutput]
  windowCountIs 2
  executeMapping mappingOutputQuit
  windowCountIs 1

test_outputQuit :: IO ()
test_outputQuit =
  tmuxSpec (svar outputSelectFirst True . svar outputAutoJump False) outputQuitSpec
