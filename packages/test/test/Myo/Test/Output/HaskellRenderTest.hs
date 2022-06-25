module Myo.Test.Output.HaskellRenderTest where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Data.Text as Text (dropWhileEnd, lines, take)
import qualified Data.Vector as Vector (fromList)
import Ribosome.Api.Buffer (currentBufferContent)
import Ribosome.Api.Syntax (executeSyntax)
import Ribosome.Data.Syntax (Syntax(..), syntaxHighlight)
import Ribosome.Msgpack.Error (DecodeError)
import Ribosome.Nvim.Api.IO (vimCommand, vimCommandOutput)
import Ribosome.Test.Screenshot (assertScreenshot)

import Myo.Command.Output (compileAndRenderReport)
import Myo.Command.Parse (storeParseResult)
import Myo.Init (initialize'')
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (LangOutputEvent(LangOutputEvent), OutputEventMeta(OutputEventMeta))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Lang.Haskell.Report (HaskellMessage(..), formatReportLine)
import Myo.Output.Lang.Haskell.Syntax (haskellSyntax)
import Myo.Output.Lang.Report (parsedOutputCons)
import Myo.Test.Config (outputAutoJump, outputSelectFirst, svar)
import Myo.Test.Unit (tmuxTest)

loc :: Location
loc =
  Location "/path/to/File.hs" 10 Nothing

event :: OutputEventMeta
event =
  OutputEventMeta (Just loc) 0

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

msg7 :: HaskellMessage
msg7 =
  DoNotationResultDiscarded "IO (Maybe Int)"

msg8 :: HaskellMessage
msg8 =
  AmbiguousTypeVar "a0" "funky" "Classy a0"

msg9 :: HaskellMessage
msg9 =
  InvalidQualifiedName "Mod.name"

msg10 :: HaskellMessage
msg10 =
  RuntimeError "Prelude.undefined"

msg11 :: HaskellMessage
msg11 =
  NonExhaustivePatterns "wrong"

msg12 :: HaskellMessage
msg12 =
  DataCtorNotInScope "Dat"

reportMsgs :: [HaskellMessage]
reportMsgs =
  [msg0, msg1, msg2, msg3, msg4, msg5, msg6, msg7, msg8, msg9, msg10, msg11, msg12]

parsedOutput :: ParsedOutput
parsedOutput =
  ParsedOutput haskellSyntax (parsedOutputCons formatReportLine events)
  where
    events =
      Vector.fromList $ LangOutputEvent event <$> reportMsgs

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
  "",
  "/path/to/File.hs \57505 11",
  "do-notation result discarded",
  "IO (Maybe Int)",
  "",
  "/path/to/File.hs \57505 11",
  "ambiguous type var for constraint",
  "a0",
  "funky",
  "Classy a0",
  "",
  "/path/to/File.hs \57505 11",
  "invalid qualified name",
  "Mod.name",
  "",
  "/path/to/File.hs \57505 11",
  "runtime error",
  "Prelude.undefined",
  "",
  "/path/to/File.hs \57505 11",
  "non-exhaustive patterns",
  "wrong",
  "",
  "/path/to/File.hs \57505 11",
  "data constructor not in scope",
  "Dat",
  ""
  ]

syntaxTarget :: [Text]
syntaxTarget =
  [
    "MyoPath        xxx match /^.*\\ze\\( \57505.*$\\)\\@=/  contained",
    "MyoLineNumber  xxx match /\\(\57505 \\)\\@<=\\zs\\d\\+\\ze/  contained",
    "MyoHsError     xxx start=/^/ end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsFoundReq,MyoHsNoInstance,MyoHsNotInScope,MyoHsModuleImport,MyoHsNameImports,MyoHsDoResDiscard,MyoHsInvalidImportName,MyoHsModuleNameMismatch,MyoHsUnknownModule,MyoHsInvalidQualifiedName,MyoHsAmbiguousTypeVar,MyoHsRuntimeError,MyoHsNonexhaustivePatterns,MyoHsDataCtorNotInScope,MyoHsNoEffect",
    "MyoLocation    xxx match /^.*\57505.*$/  contains=MyoPath,MyoLineNumber nextgroup=MyoHsError skipwhite skipnl",
    "MyoHsFoundReq  xxx start=/type mismatch/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsFound",
    "MyoHsNoInstance xxx start=/\\s*!instance:/ end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsNoInstanceHead",
    "MyoHsNotInScope xxx start=/\\%(variable\\|type\\) not in scope/ end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsNotInScopeHead",
    "MyoHsModuleImport xxx start=/redundant module import/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsModule",
    "MyoHsNameImports xxx start=/redundant name imports/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsNames",
    "MyoHsDoResDiscard xxx start=/do-notation result discarded/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsCode",
    "MyoHsInvalidImportName xxx start=/invalid import name/ end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsInvalidImportNameHead",
    "MyoHsModuleNameMismatch xxx start=/module name mismatch/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsFound",
    "MyoHsUnknownModule xxx start=/unknown module/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsUnknownModuleHead",
    "MyoHsInvalidQualifiedName xxx start=/invalid qualified name/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsCode",
    "MyoHsAmbiguousTypeVar xxx start=/ambiguous type var for constraint/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsAmbiguousTypeVarVar",
    "MyoHsRuntimeError xxx start=/runtime error/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsCode",
    "MyoHsNonexhaustivePatterns xxx start=/non-exhaustive patterns/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsCode",
    "MyoHsDataCtorNotInScope xxx start=/data constructor not in scope/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsCode",
    "MyoHsNoEffect  xxx start=/\\s*!effect:/ end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsNoEffectHead",
    "MyoHsFound     xxx match /^.*$/  contained nextgroup=MyoHsReq skipnl",
    "MyoHsKindMismatch xxx start=/kind mismatch/ms=e+1 end=/\\v\\ze.*(\57505|\8224)/  contained contains=MyoHsFound",
    "MyoHsReq       xxx match /^.*$/  contained",
    "MyoHsCode      xxx match /.*/  contained keepend contains=@haskell",
    "MyoHsNoInstanceHead xxx match /\\s*\\s*!instance:.*$/  contained contains=MyoHsNoInstanceBang nextgroup=MyoHsNoInstanceDesc skipnl",
    "MyoHsNoInstanceBang xxx match /!/  contained nextgroup=MyoHsNoInstanceKw",
    "MyoHsNoInstanceDesc xxx match /.*/  contained keepend contains=@haskell",
    "MyoHsNoInstanceKw xxx match /instance\\ze:/  contained nextgroup=MyoHsNoInstanceTrigger skipwhite",
    "MyoHsNoInstanceTrigger xxx match /.*/  contained keepend contains=@haskell nextgroup=MyoHsNoInstanceDesc skipnl",
    "MyoHsNotInScopeHead xxx match /\\s*\\%(variable\\|type\\) not in scope/  contained nextgroup=MyoHsCode skipnl",
    "MyoHsModule    xxx match /^.*$/  contained",
    "MyoHsNames     xxx match /^.*$/  contained contains=MyoHsName nextgroup=MyoHsModule skipnl",
    "MyoHsName      xxx match /\\w\\+/  contained",
    "MyoHsInvalidImportNameHead xxx match /invalid import name/  contained nextgroup=MyoHsNames skipnl",
    "MyoHsUnknownModuleHead xxx match /unknown module/  contained nextgroup=MyoHsModule skipnl",
    "MyoHsAmbiguousTypeVarVar xxx match /^.*$/  contained nextgroup=MyoHsAmbiguousTypeVarMethod skipwhite skipnl",
    "MyoHsAmbiguousTypeVarMethod xxx match /^.*$/  contained nextgroup=MyoHsCode skipwhite skipnl",
    "MyoHsNoEffectHead xxx match /\\s*\\s*!effect:.*$/  contained contains=MyoHsNoEffectBang",
    "MyoHsNoEffectKw xxx match /effect\\ze:/  contained nextgroup=MyoHsNoEffectEffect skipwhite",
    "MyoHsNoEffectBang xxx match /!/  contained nextgroup=MyoHsNoEffectKw",
    "MyoHsNoEffectEffect xxx match /.*$/  contained",
    "MyoPath        xxx links to Directory",
    "MyoLineNumber  xxx links to Directory",
    "MyoHsError     xxx links to Error",
    "MyoLocation    xxx cleared",
    "MyoHsFoundReq  xxx cleared",
    "MyoHsNoInstance xxx cleared",
    "MyoHsNotInScope xxx cleared",
    "MyoHsModuleImport xxx cleared",
    "MyoHsNameImports xxx cleared",
    "MyoHsDoResDiscard xxx cleared",
    "MyoHsInvalidImportName xxx cleared",
    "MyoHsModuleNameMismatch xxx cleared",
    "MyoHsUnknownModule xxx cleared",
    "MyoHsInvalidQualifiedName xxx cleared",
    "MyoHsAmbiguousTypeVar xxx cleared",
    "MyoHsRuntimeError xxx cleared",
    "MyoHsNonexhaustivePatterns xxx cleared",
    "MyoHsDataCtorNotInScope xxx cleared",
    "MyoHsNoEffect  xxx cleared",
    "MyoHsFound     xxx ctermfg=1 guifg=#dc322f",
    "MyoHsKindMismatch xxx cleared",
    "MyoHsReq       xxx ctermfg=2 guifg=#719e07",
    "MyoHsCode      xxx cleared",
    "MyoHsNoInstanceHead xxx cleared",
    "MyoHsNoInstanceBang xxx links to Error",
    "MyoHsNoInstanceDesc xxx cleared",
    "MyoHsNoInstanceKw xxx links to Directory",
    "MyoHsNoInstanceTrigger xxx ctermfg=3",
    "MyoHsNotInScopeHead xxx links to Error",
    "MyoHsModule    xxx links to Type",
    "MyoHsNames     xxx cleared",
    "MyoHsName      xxx ctermfg=5 guifg=#d33682",
    "MyoHsInvalidImportNameHead xxx links to Error",
    "MyoHsUnknownModuleHead xxx links to Error",
    "MyoHsAmbiguousTypeVarVar xxx links to MyoHsName",
    "MyoHsAmbiguousTypeVarMethod xxx links to MyoHsCode",
    "MyoHsNoEffectHead xxx cleared",
    "MyoHsNoEffectKw xxx links to Directory",
    "MyoHsNoEffectBang xxx links to Error",
    "MyoHsNoEffectEffect xxx links to Type",
    "MyoHsDoResDiscardHead xxx links to Error",
    "MyoHsModuleNameMismatchHead xxx links to Error",
    "MyoHsInvalidQualifiedNameHead xxx links to Error",
    "MyoHsAmbiguousTypeVarHead xxx links to Error"
    ]

setupHighlights ::
  m ()
setupHighlights =
  void $ executeSyntax (Syntax [] hls [])
  where
    hls = [
      syntaxHighlight "Error" [("ctermfg", "1"), ("cterm", "bold")],
      syntaxHighlight "Type" [("ctermfg", "3")]
      ]

myoSyntax = do
  syntax <- parse <$> vimCommandOutput "syntax"
  hi <- parse <$> vimCommandOutput "hi"
  pure $ syntax <> hi
  where
    parse = fmap (Text.dropWhileEnd (' ' ==)) <$> filter isMyo . Text.lines
    isMyo item = Text.take 3 item == "Myo"

haskellRenderTest :: Sem r ()
haskellRenderTest = do
  lift initialize''
  setupHighlights
  storeParseResult (Ident.Str "test") [parsedOutput]
  compileAndRenderReport
  vimCommand "wincmd w"
  vimCommand "wincmd o"
  (target ===) =<< currentBufferContent
  (syntaxTarget ===) =<< myoSyntax
  assertScreenshot "render-haskell-parse-result" False 0

test_haskellRender :: UnitTest
test_haskellRender =
  tmuxTest (svar outputSelectFirst True . svar outputAutoJump False) haskellRenderTest
