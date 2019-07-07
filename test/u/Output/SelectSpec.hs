{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.SelectSpec (htf_thisModulesTests) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, zipWith)
import Ribosome.Api.Window (currentCursor)
import Ribosome.Plugin.Mapping (executeMapping)
import Ribosome.Test.Ui (windowCountIs)
import Ribosome.Test.Unit (fixture)
import System.FilePath ((</>))
import Test.Framework

import Config (outputAutoJump, outputSelectFirst, svar)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (parseResult)
import Myo.Command.Output (renderParseResult)
import Myo.Data.Env (Myo)
import Myo.Init (initialize'')
import qualified Myo.Output.Data.EventIndex as EventIndex (Relative)
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (LangOutputEvent(LangOutputEvent), OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParseResult (ParseResult(ParseResult))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine)
import Myo.Output.Lang.Haskell.Report (HaskellMessage(FoundReq1, NoMethod), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Plugin (mappingOutputSelect)
import Unit (tmuxSpec)

events :: Text -> Vector OutputEvent
events file =
  Vector.fromList [OutputEvent (Just (Location file 9 (Just 2))) 0, OutputEvent Nothing 1]

loc :: Text -> Location
loc file =
  Location file 10 Nothing

messages :: Vector HaskellMessage
messages =
  Vector.fromList [FoundReq1 "TypeA" "TypeB", NoMethod "fmap"]

parsedOutput :: Text -> ParsedOutput
parsedOutput file =
  ParsedOutput haskellSyntax (parsedOutputCons formatReportLine (Vector.zipWith LangOutputEvent (events file) messages))

outputSelectSpec :: Myo ()
outputSelectSpec = do
  file <- fixture $ "output" </> "select" </> "File.hs"
  let po = [parsedOutput (toText file)]
  initialize''
  setL @CommandState CommandState.parseResult (Just (ParseResult (Ident.Str "test") po))
  renderParseResult (Ident.Str "test") po
  windowCountIs 2
  executeMapping mappingOutputSelect
  (line, col) <- currentCursor
  gassertEqual (9, 2) (line, col)

test_outputSelect :: IO ()
test_outputSelect =
  tmuxSpec (svar outputSelectFirst True . svar outputAutoJump False) outputSelectSpec
