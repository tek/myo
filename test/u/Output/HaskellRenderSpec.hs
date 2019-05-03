{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.HaskellRenderSpec (htf_thisModulesTests) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Data.Functor (void)
import Data.Functor.Syntax ((<$$>))
import qualified Data.Text as Text (dropWhileEnd, lines, take)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Syntax (executeSyntax)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Data.Syntax (Syntax(..), syntaxHighlight)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCommand, vimCommandOutput)
import Ribosome.Test.Screenshot (assertScreenshot)
import Test.Framework

import Config (outputAutoJump, outputSelectFirst, svar)
import Myo.Command.Output (renderParseResult)
import Myo.Data.Env (Myo)
import Myo.Init (initialize'')
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine)
import Myo.Output.Data.String (lineNumber)
import Myo.Output.Lang.Haskell.Report (HaskellMessage(..), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Unit (tmuxSpec)

loc :: Location
loc =
  Location "/path/to/File.hs" 10 Nothing

events :: Vector OutputEvent
events =
  Vector.fromList [OutputEvent (Just loc) 0, OutputEvent (Just loc) 1]

reportLines :: Vector ReportLine
reportLines =
  formatReportLine 0 loc (FoundReq1 "TypeA" "TypeB") <> formatReportLine 1 loc (NoMethod "fmap")

parsedOutput :: ParsedOutput
parsedOutput =
  ParsedOutput haskellSyntax (const $ ParseReport events reportLines)

target :: [Text]
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

syntaxTarget :: [Text]
syntaxTarget = [
  "MyoPath        xxx match /^.*\\ze\\( " <> lineNumber <> ".*$\\)\\@=/  contained containedin=MyoLocation",
  "MyoLineNumber  xxx match /\\(" <> lineNumber <> " \\)\\@<=\\zs\\d\\+\\ze/  contained containedin=MyoLocation",
  "MyoHsError     xxx start=/^/ end=/\\ze.*\\(" <> lineNumber <> "\\|†\\)/  contained contains=MyoHsFoundReq,MyoHsNoInstance,MyoHsNotInScope",
  "MyoLocation    xxx match /^.*" <> lineNumber <> ".*$/  contains=MyoPath,MyoLineNumber nextgroup=MyoHsError skipwhite skipnl",
  "MyoHsFoundReq  xxx start=/type mismatch/ end=/\\ze.*\\(" <> lineNumber <> "\\|†\\)/  contained contains=MyoHsFound",
  "MyoHsNoInstance xxx start=/\\s*!instance:/ end=/\\ze.*\\(" <> lineNumber <> "\\|†\\)/  contained contains=MyoHsNoInstanceHead",
  "MyoHsNotInScope xxx start=/Variable not in scope:/ end=/\\ze.*\\(" <> lineNumber <> "\\|†\\)/  contained contains=MyoHsNotInScopeHead",
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

setupHighlights ::
  MonadDeepError e DecodeError m =>
  NvimE e m =>
  m ()
setupHighlights =
  void $ executeSyntax (Syntax [] [syntaxHighlight "Error" [("ctermfg", "1"), ("cterm", "bold")]] [])

myoSyntax :: NvimE e m => m [Text]
myoSyntax = do
  syntax <- parse <$> vimCommandOutput "syntax"
  hi <- parse <$> vimCommandOutput "hi"
  return $ syntax <> hi
  where
    parse = Text.dropWhileEnd (' ' ==) <$$> filter isMyo . Text.lines
    isMyo item = Text.take 3 item == "Myo"

haskellRenderSpec :: Myo ()
haskellRenderSpec = do
  initialize''
  setupHighlights
  renderParseResult (Ident.Str "test") [parsedOutput]
  vimCommand "wincmd w"
  content <- currentBufferContent
  gassertEqual target content
  syntax <- myoSyntax
  gassertEqual syntaxTarget syntax
  assertScreenshot "render-haskell-parse-result" False 0

test_haskellRender :: IO ()
test_haskellRender =
  tmuxSpec (svar outputSelectFirst True . svar outputAutoJump False) haskellRenderSpec
