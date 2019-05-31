{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE QuasiQuotes #-}

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
import Myo.Output.Lang.Haskell.Report (HaskellMessage(..), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Text.RawString.QQ (r)
import Unit (tmuxSpec)

loc :: Location
loc =
  Location "/path/to/File.hs" 10 Nothing

events :: Vector OutputEvent
events =
  Vector.fromList $ OutputEvent (Just loc) <$> [0..4]

msg0 :: HaskellMessage
msg0 =
  FoundReq1 "TypeA" "TypeB"

msg1 :: HaskellMessage
msg1 =
  NoMethod "fmap"

msg2 :: HaskellMessage
msg2 =
  ModuleImport "Data.Structure.Strict"

msg3 :: HaskellMessage
msg3 =
  NamesImport "Data.Structure.Strict" ["NameA", "NameB(Ctor1, fun)", "NameC"]

msg4 :: HaskellMessage
msg4 =
  NoInstance "MonadIO (t m)" "run"

msg5 :: HaskellMessage
msg5 =
  TypeNotInScope "Unknown.Type"

msg6 :: HaskellMessage
msg6 =
  VariableNotInScope "var" "IO a0"

reportMsgs :: [HaskellMessage]
reportMsgs =
  [msg0, msg1, msg2, msg3, msg4, msg5, msg6]

reportLine :: Int -> HaskellMessage -> Vector ReportLine
reportLine index =
  formatReportLine index loc

parsedOutput :: ParsedOutput
parsedOutput =
  ParsedOutput haskellSyntax (const $ ParseReport events lines')
  where
    lines' =
      uncurry reportLine =<< Vector.fromList (zip [0..] reportMsgs)

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
  "",
  "/path/to/File.hs \57505 11",
  "!instance: run",
  "MonadIO (t m)",
  "",
  "/path/to/File.hs \57505 11",
  "type not in scope",
  "Unknown.Type",
  "",
  "/path/to/File.hs \57505 11",
  "variable not in scope",
  "var :: IO a0",
  ""
  ]

syntaxTarget :: [Text]
syntaxTarget = [
    [r|MyoPath        xxx match /^.*\ze\( .*$\)\@=/  contained containedin=MyoLocation|],
    [r|MyoLineNumber  xxx match /\( \)\@<=\zs\d\+\ze/  contained containedin=MyoLocation|],
    [r|MyoHsError     xxx start=/^/ end=/\ze.*\(\|†\)/  contained contains=MyoHsFoundReq,MyoHsNoInstance,MyoHsNotInScope,MyoHsModuleImport,MyoHsNameImports|],
    "MyoLocation    xxx match /^.*\57505.*$/  contains=MyoPath,MyoLineNumber nextgroup=MyoHsError skipwhite skipnl",
    [r|MyoHsFoundReq  xxx start=/type mismatch/ms=e+1 end=/\ze.*\(\|†\)/  contained contains=MyoHsFound|],
    [r|MyoHsNoInstance xxx start=/\s*!instance:/ end=/\ze.*\(\|†\)/  contained contains=MyoHsNoInstanceHead|],
    [r|MyoHsNotInScope xxx start=/\%(variable\|type\) not in scope/ end=/\ze.*\(\|†\)/  contained contains=MyoHsNotInScopeHead|],
    [r|MyoHsModuleImport xxx start=/redundant module import/ms=e+1 end=/\ze.*\(\|†\)/  contained contains=MyoHsModule|],
    [r|MyoHsNameImports xxx start=/redundant name imports/ms=e+1 end=/\ze.*\(\|†\)/  contained contains=MyoHsNames|],
    "MyoHsFound     xxx match /^.*$/  contained nextgroup=MyoHsReq skipnl",
    "MyoHsReq       xxx match /^.*$/  contained",
    "MyoHsCode      xxx match /.*/  contained contains=@haskell",
    [r|MyoHsNoInstanceHead xxx match /\s*\s*!instance:.*$/  contained contains=MyoHsNoInstanceBang nextgroup=MyoHsNoInstanceDesc skipnl|],
    "MyoHsNoInstanceBang xxx match /!/  contained nextgroup=MyoHsNoInstanceKw",
    "MyoHsNoInstanceDesc xxx match /.*/  contained keepend contains=@haskell",
    "MyoHsNoInstanceKw xxx match /instance\\ze:/  contained nextgroup=MyoHsNoInstanceTrigger skipwhite",
    "MyoHsNoInstanceTrigger xxx match /.*/  contained keepend contains=@haskell nextgroup=MyoHsNoInstanceDesc skipnl",
    [r|MyoHsNotInScopeHead xxx match /\s*\%(variable\|type\) not in scope/  contained nextgroup=MyoHsCode skipnl|],
    "MyoHsModule    xxx match /^.*$/  contained",
    "MyoHsNames     xxx match /^.*$/  contained contains=MyoHsName nextgroup=MyoHsModule skipnl",
    [r|MyoHsName      xxx match /\w\+/  contained|],
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
