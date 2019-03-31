{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.HaskellRenderSpec(
  htf_thisModulesTests,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Syntax (executeSyntax)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.Syntax (Syntax(..), syntaxHighlight)
import Ribosome.Nvim.Api.IO (vimCommandOutput)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Screenshot (assertScreenshot)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Ribosome.Tmux.Run (runTmux)
import Test.Framework

import Myo.Command.Output (renderParseResult)
import Myo.Data.Env (MyoN)
import Myo.Init (initialize'')
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))
import Myo.Output.Lang.Haskell.Report (HaskellMessage(..), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)

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
  ParsedOutput haskellSyntax (const $ ParseReport events reportLines)

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

setupHighlights :: NvimE e m => m ()
setupHighlights =
  void $ executeSyntax (Syntax [] [syntaxHighlight "Error" [("ctermfg", "1"), ("cterm", "bold")]] [])

myoSyntax :: NvimE e m => m [String]
myoSyntax =
  filter isMyo . lines <$> vimCommandOutput "syntax"
  where
    isMyo item = take 3 item == "Myo"

haskellRenderSpec :: MyoN ()
haskellRenderSpec = do
  initialize''
  setupHighlights
  renderParseResult [parsedOutput]
  content <- currentBufferContent
  gassertEqual target content
  sleep 2
  s <- myoSyntax
  liftIO $ traverse putStrLn s
  assertScreenshot "render-haskell-parse-result" False 0

test_haskellRender :: IO ()
test_haskellRender =
  tmuxGuiSpecDef haskellRenderSpec
