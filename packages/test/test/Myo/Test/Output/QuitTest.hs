module Myo.Test.Output.QuitTest where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, zipWith)
import Ribosome.Plugin.Mapping (executeMapping)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Ui (windowCountIs)

import Myo.Command.Output (compileAndRenderReport)
import Myo.Command.Parse (storeParseResult)
import Myo.Init (initialize'')
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (LangOutputEvent(LangOutputEvent), OutputEventMeta(OutputEventMeta))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Lang.Haskell.Report (HaskellMessage(FoundReq1, NoMethod), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Plugin (mappingOutputQuit)
import Myo.Test.Config (outputAutoJump, outputSelectFirst, svar)
import Myo.Test.Unit (MyoTest, tmuxTest)

loc :: Location
loc =
  Location "/path/to/File.hs" 10 Nothing

events :: Vector OutputEventMeta
events =
  Vector.fromList [OutputEventMeta (Just loc) 0, OutputEventMeta (Just loc) 1]

messages :: Vector HaskellMessage
messages =
  Vector.fromList [FoundReq1 "TypeA" "TypeB", NoMethod "fmap"]

parsedOutput :: ParsedOutput
parsedOutput =
  ParsedOutput haskellSyntax (parsedOutputCons formatReportLine (Vector.zipWith LangOutputEvent events messages))

outputQuitTest :: MyoTest ()
outputQuitTest = do
  lift initialize''
  storeParseResult (Ident.Str "test") [parsedOutput]
  compileAndRenderReport
  windowCountIs 2
  executeMapping mappingOutputQuit
  windowCountIs 1

test_outputQuit :: UnitTest
test_outputQuit =
  tmuxTest (svar outputSelectFirst True . svar outputAutoJump False) outputQuitTest
