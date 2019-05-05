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
  Vector.fromList $ OutputEvent (Just loc) <$> [0..4]

line0 :: Vector ReportLine
line0 =
  formatReportLine 0 loc (FoundReq1 "TypeA" "TypeB")

line1 :: Vector ReportLine
line1 =
  formatReportLine 1 loc (NoMethod "fmap")

line2 :: Vector ReportLine
line2 =
  formatReportLine 2 loc (ModuleImport "Data.Structure.Strict")

line3 :: Vector ReportLine
line3 =
  formatReportLine 3 loc (NamesImport "Data.Structure.Strict" ["NameA", "NameB(Ctor1, fun)", "NameC"])

reportLines :: Vector ReportLine
reportLines =
  line0 <> line1 <> line2 <> line3

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
  "",
  "/path/to/File.hs \57505 11",
  "redundant module import",
  "Data.Structure.Strict",
  "",
  "/path/to/File.hs \57505 11",
  "redundant name imports",
  "NameA, NameB(Ctor1, fun), NameC",
  "Data.Structure.Strict",
  ""
  ]

syntaxTarget :: [Text]
syntaxTarget = [
    "MyoPath        xxx match /^.*\\ze\\( \57505.*$\\)\\@=/  contained containedin=MyoLocation",
    "MyoLineNumber  xxx match /\\(\57505 \\)\\@<=\\zs\\d\\+\\ze/  contained containedin=MyoLocation",
    "MyoHsError     xxx start=/^/ end=/\\ze.*\\(\57505\\|\8224\\)/  contained contains=MyoHsFoundReq,MyoHsNoInstance,MyoHsNotInScope,MyoHsModuleImport,MyoHsNameImports",
    "MyoLocation    xxx match /^.*\57505.*$/  contains=MyoPath,MyoLineNumber nextgroup=MyoHsError skipwhite skipnl",
    "MyoHsFoundReq  xxx start=/type mismatch/ms=e+1 end=/\\ze.*\\(\57505\\|\8224\\)/  contained contains=MyoHsFound",
    "MyoHsNoInstance xxx start=/\\s*!instance:/ end=/\\ze.*\\(\57505\\|\8224\\)/  contained contains=MyoHsNoInstanceHead",
    "MyoHsNotInScope xxx start=/Variable not in scope:/ end=/\\ze.*\\(\57505\\|\8224\\)/  contained contains=MyoHsNotInScopeHead",
    "MyoHsModuleImport xxx start=/redundant module import/ms=e+1 end=/\\ze.*\\(\57505\\|\8224\\)/  contained contains=MyoHsModule",
    "MyoHsNameImports xxx start=/redundant name imports/ms=e+1 end=/\\ze.*\\(\57505\\|\8224\\)/  contained contains=MyoHsNames",
    "MyoHsFound     xxx match /^.*$/  contained nextgroup=MyoHsReq skipnl",
    "MyoHsReq       xxx match /^.*$/  contained",
    "MyoHsCode      xxx match /.*/  contained contains=@haskell",
    "MyoHsNoInstanceHead xxx match /\\s*\\s*!instance:.*$/  contained contains=MyoHsNoInstanceBang nextgroup=MyoHsNoInstanceDesc skipnl",
    "MyoHsNoInstanceBang xxx match /!/  contained nextgroup=MyoHsNoInstanceKw",
    "MyoHsNoInstanceDesc xxx match /.*/  contained contains=@haskell nextgroup=MyoLocation skipnl",
    "MyoHsNoInstanceKw xxx match /instance\\ze:/  contained nextgroup=MyoHsNoInstanceTrigger skipwhite",
    "MyoHsNoInstanceTrigger xxx match /.*/  contained",
    "MyoHsNotInScopeHead xxx match /\\s*Variable not in scope:/  contained nextgroup=MyoHsCode skipnl",
    "MyoHsModule    xxx match /^.*$/  contained",
    "MyoHsNames     xxx match /^.*$/  contained contains=MyoHsName nextgroup=MyoHsModule skipnl",
    "MyoHsName      xxx match /\\w\\+/  contained",
    "MyoPath        xxx links to Directory",
    "MyoLineNumber  xxx links to Directory",
    "MyoHsError     xxx links to Error", "MyoLocation    xxx cleared",
    "MyoHsFoundReq  xxx cleared", "MyoHsNoInstance xxx cleared",
    "MyoHsNotInScope xxx cleared", "MyoHsModuleImport xxx cleared",
    "MyoHsNameImports xxx cleared",
    "MyoHsFound     xxx ctermfg=1 guifg=#dc322f",
    "MyoHsReq       xxx ctermfg=2 guifg=#719e07",
    "MyoHsCode      xxx cleared", "MyoHsNoInstanceHead xxx cleared",
    "MyoHsNoInstanceBang xxx links to Error",
    "MyoHsNoInstanceDesc xxx cleared",
    "MyoHsNoInstanceKw xxx links to Directory",
    "MyoHsNoInstanceTrigger xxx ctermfg=3",
    "MyoHsNotInScopeHead xxx links to Error",
    "MyoHsModule    xxx links to Type", "MyoHsNames     xxx cleared",
    "MyoHsName      xxx ctermfg=5 guifg=#d33682"
  ]

setupHighlights ::
  MonadDeepError e DecodeError m =>
  NvimE e m =>
  m ()
setupHighlights = do
  void $ executeSyntax (Syntax [] [syntaxHighlight "Error" [("ctermfg", "1"), ("cterm", "bold")]] [])
  void $ executeSyntax (Syntax [] [syntaxHighlight "Type" [("ctermfg", "3")]] [])

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
