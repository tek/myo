{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.HaskellRenderSpec(
  htf_thisModulesTests,
) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Functor.Syntax ((<$$>))
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Syntax (executeSyntax)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.Syntax (Syntax(..), syntaxHighlight)
import Ribosome.Nvim.Api.IO (vimCommandOutput)
import Ribosome.System.Time (sleep)
import Ribosome.Test.Screenshot (assertScreenshot)
import Ribosome.Test.Tmux (tmuxGuiSpecDef)
import Test.Framework

import Myo.Command.Output (renderParseResult)
import Myo.Data.Env (MyoN)
import Myo.Init (initialize'')
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ReportLine (ReportLine(ReportLine))
import Myo.Output.Data.String (lineNumber)
import Myo.Output.Lang.Haskell.Report (formatReportLine, HaskellMessage(..))
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

syntaxTarget :: [String]
syntaxTarget = [
  "MyoPath        xxx match /^.*\\ze\\( " ++ lineNumber ++ ".*$\\)\\@=/  contained containedin=MyoLocation",
  "MyoLineNumber  xxx match /\\(" ++ lineNumber ++ " \\)\\@<=\\zs\\d\\+\\ze/  contained containedin=MyoLocation",
  "MyoHsError     xxx start=/^/ end=/\\ze.*\\(" ++ lineNumber ++ "\\|†\\)/  contained contains=MyoHsFoundReq,MyoHsNoInstance,MyoHsNotInScope",
  "MyoLocation    xxx match /^.*" ++ lineNumber ++ ".*$/  contains=MyoPath,MyoLineNumber nextgroup=MyoHsError skipwhite skipnl",
  "MyoHsFoundReq  xxx start=/type mismatch/ end=/\\ze.*\\(" ++ lineNumber ++ "\\|†\\)/  contained contains=MyoHsFound",
  "MyoHsNoInstance xxx start=/\\s*!instance:/ end=/\\ze.*\\(" ++ lineNumber ++ "\\|†\\)/  contained contains=MyoHsNoInstanceHead",
  "MyoHsNotInScope xxx start=/Variable not in scope:/ end=/\\ze.*\\(" ++ lineNumber ++ "\\|†\\)/  contained contains=MyoHsNotInScopeHead",
  "MyoHsFound     xxx match /^.*$/  contained nextgroup=MyoHsReq skipnl",
  "MyoHsReq       xxx match /^.*$/  contained",
  "MyoHsCode      xxx match /.*/  contained contains=@haskell",
  "MyoHsNoInstanceHead xxx match /\\s*\\s*!instance:.*$/  contained contains=MyoHsNoInstanceBang nextgroup=MyoHsNoInstanceDesc skipnl",
  "MyoHsNoInstanceBang xxx match /!/  contained nextgroup=MyoHsNoInstanceKw",
  "MyoHsNoInstanceDesc xxx match /.*/  contained contains=@haskell nextgroup=MyoLocation skipnl",
  "MyoHsNoInstanceKw xxx match /instance\\ze:/  contained nextgroup=MyoHsNoInstanceTrigger skipwhite",
  "MyoHsNoInstanceTrigger xxx match /.*/  contained",
  "MyoHsNotInScopeHead xxx match /\\s*Variable not in scope:/  contained nextgroup=MyoHsCode skipnl",
  "MyoPath        xxx links to Directory",
  "MyoLineNumber  xxx links to Directory",
  "MyoHsError     xxx cleared",
  "MyoLocation    xxx cleared",
  "MyoHsFoundReq  xxx links to Title",
  "MyoHsNoInstance xxx cleared",
  "MyoHsNotInScope xxx cleared",
  "MyoHsFound     xxx links to Error",
  "MyoHsReq       xxx ctermfg=2 guifg=#719e07",
  "MyoHsCode      xxx cleared",
  "MyoHsNoInstanceHead xxx cleared",
  "MyoHsNoInstanceBang xxx links to Error",
  "MyoHsNoInstanceDesc xxx cleared",
  "MyoHsNoInstanceKw xxx links to Directory",
  "MyoHsNoInstanceTrigger xxx ctermfg=3",
  "MyoHsNotInScopeHead xxx links to Error"
  ]

setupHighlights :: NvimE e m => m ()
setupHighlights =
  void $ executeSyntax (Syntax [] [syntaxHighlight "Error" [("ctermfg", "1"), ("cterm", "bold")]] [])

myoSyntax :: NvimE e m => m [String]
myoSyntax = do
  syntax <- parse <$> vimCommandOutput "syntax"
  hi <- parse <$> vimCommandOutput "hi"
  return $ syntax ++ hi
  where
    parse = reverse . dropWhile (' ' ==) . reverse <$$> filter isMyo . lines
    isMyo item = take 3 item == "Myo"

haskellRenderSpec :: MyoN ()
haskellRenderSpec = do
  initialize''
  setupHighlights
  renderParseResult (Ident.Str "test") [parsedOutput]
  content <- currentBufferContent
  gassertEqual target content
  syntax <- myoSyntax
  gassertEqual syntaxTarget syntax
  assertScreenshot "render-haskell-parse-result" False 0

test_haskellRender :: IO ()
test_haskellRender =
  tmuxGuiSpecDef haskellRenderSpec
