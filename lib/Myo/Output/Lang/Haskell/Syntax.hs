{-# LANGUAGE QuasiQuotes #-}

module Myo.Output.Lang.Haskell.Syntax where

import qualified Data.Map as Map (fromList)
import Data.String.QM (qt)
import Ribosome.Data.Syntax (
  HiLink(..),
  Highlight(..),
  Syntax(Syntax),
  SyntaxItem(..),
  syntaxHighlight,
  syntaxMatch,
  syntaxRegion,
  syntaxRegionOffset,
  syntaxVerbatim,
  )
import Text.RawString.QQ (r)

import Myo.Output.Data.String (colMarker, lineNumber)

errorEnd :: Text
errorEnd =
  [qt|\\ze.*\\(${lineNumber}\\|${colMarker}\\)|]

foundReqMarker :: Text
foundReqMarker =
  "type mismatch"

noInstanceMarker :: Text
noInstanceMarker =
  "\\s*!instance:"

notInScopeMarker :: Text
notInScopeMarker = [r|\%(variable\|type\) not in scope|]

moduleImportMarker :: Text
moduleImportMarker =
  "redundant module import"

nameImportsMarker :: Text
nameImportsMarker =
  "redundant name imports"

doResDiscardMarker :: Text
doResDiscardMarker =
  "do-notation result discarded"

invalidImportNameMarker :: Text
invalidImportNameMarker =
  "invalid import name"

moduleNameMismatchMarker :: Text
moduleNameMismatchMarker =
  "module name mismatch"

unknownModuleMarker :: Text
unknownModuleMarker =
  "unknown module"

ambiguousTypeVarMarker :: Text
ambiguousTypeVarMarker =
  "ambiguous type var for constraint"

invalidQualifiedNameMarker :: Text
invalidQualifiedNameMarker =
  "invalid qualified name"

runtimeErrorMarker :: Text
runtimeErrorMarker =
  "runtime error"

patternsMarker :: Text
patternsMarker =
  "non-exhaustive patterns"

dataCtorNotInScopeMarker :: Text
dataCtorNotInScopeMarker =
  "data constructor not in scope"

haskellInclude :: SyntaxItem
haskellInclude =
  syntaxVerbatim "syntax include @haskell syntax/haskell.vim"

location :: SyntaxItem
location =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoLocation" ("^.*" <> lineNumber <> ".*$")
    options = ["skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoPath,MyoLineNumber"), ("nextgroup", "MyoHsError")]

path :: SyntaxItem
path =
  item { siOptions = options }
  where
    item = syntaxMatch "MyoPath" ("^.*\\ze\\( " <> lineNumber <> ".*$\\)\\@=")
    options = ["contained"]

lineNumberSymbol :: SyntaxItem
lineNumberSymbol =
  item { siOptions = options }
  where
    item = syntaxMatch "MyoLineNumber" ("\\(" <> lineNumber <> " \\)\\@<=\\zs\\d\\+\\ze")
    options = ["contained"]

errorMessage :: SyntaxItem
errorMessage =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegion "MyoHsError" "^" errorEnd Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", contains1 <> contains2 <> contains3)]
    contains1 =
      "MyoHsFoundReq,MyoHsNoInstance,MyoHsNotInScope,MyoHsModuleImport,MyoHsNameImports,MyoHsDoResDiscard"
    contains2 =
      ",MyoHsInvalidImportName,MyoHsModuleNameMismatch,MyoHsUnknownModule,MyoHsInvalidQualifiedName"
    contains3 =
      ",MyoHsAmbiguousTypeVar,MyoHsRuntimeError,MyoHsNonexhaustivePatterns,MyoHsDataCtorNotInScope"

foundReq :: SyntaxItem
foundReq =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegionOffset "MyoHsFoundReq" foundReqMarker errorEnd Nothing "ms=e+1" ""
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsFound")]

found :: SyntaxItem
found =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsFound" "^.*$"
    options = ["contained", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsReq")]

req :: SyntaxItem
req =
  item { siOptions = options }
  where
    item = syntaxMatch "MyoHsReq" "^.*$"
    options = ["contained"]

code :: SyntaxItem
code =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsCode" ".*"
    options = ["contained"]
    params = Map.fromList [("contains", "@haskell")]

noInstance :: SyntaxItem
noInstance =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegion "MyoHsNoInstance" noInstanceMarker errorEnd Nothing
    options = ["contained"]
    params = Map.fromList [("contains", "MyoHsNoInstanceHead")]

noInstanceHead :: SyntaxItem
noInstanceHead =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsNoInstanceHead" ("\\s*" <> noInstanceMarker <> ".*$")
    options = ["contained", "skipnl"]
    params = Map.fromList [("contains", "MyoHsNoInstanceBang"), ("nextgroup", "MyoHsNoInstanceDesc")]

noInstanceBang :: SyntaxItem
noInstanceBang =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsNoInstanceBang" "!"
    options = ["contained"]
    params = Map.fromList [("nextgroup", "MyoHsNoInstanceKw")]

noInstanceKw :: SyntaxItem
noInstanceKw =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsNoInstanceKw" "instance\\ze:"
    options = ["contained", "skipwhite"]
    params = Map.fromList [("nextgroup", "MyoHsNoInstanceTrigger")]

noInstanceTrigger :: SyntaxItem
noInstanceTrigger =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsNoInstanceTrigger" ".*"
    options = ["contained", "skipnl", "keepend"]
    params = Map.fromList [("contains", "@haskell"), ("nextgroup", "MyoHsNoInstanceDesc")]

noInstanceDesc :: SyntaxItem
noInstanceDesc =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsNoInstanceDesc" ".*"
    options = ["contained", "skipnl", "keepend"]
    params = Map.fromList [("contains", "@haskell")]

notInScope :: SyntaxItem
notInScope =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegion "MyoHsNotInScope" notInScopeMarker errorEnd Nothing
    options = ["contained"]
    params = Map.fromList [("contains", "MyoHsNotInScopeHead")]

notInScopeHead :: SyntaxItem
notInScopeHead =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsNotInScopeHead" ("\\s*" <> notInScopeMarker)
    options = ["contained", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsCode")]

moduleImport :: SyntaxItem
moduleImport =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegionOffset "MyoHsModuleImport" moduleImportMarker errorEnd Nothing "ms=e+1" ""
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsModule")]

nameImports :: SyntaxItem
nameImports =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegionOffset "MyoHsNameImports" nameImportsMarker errorEnd Nothing "ms=e+1" ""
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsNames")]

names :: SyntaxItem
names =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsNames" "^.*$"
    options = ["contained", "skipnl"]
    params = Map.fromList [("contains", "MyoHsName"), ("nextgroup", "MyoHsModule")]

name :: SyntaxItem
name =
  item { siOptions = options }
  where
    item = syntaxMatch "MyoHsName" "\\w\\+"
    options = ["contained"]

moduleLine :: SyntaxItem
moduleLine =
  item { siOptions = options }
  where
    item = syntaxMatch "MyoHsModule" "^.*$"
    options = ["contained", "skipnl"]

simpleMessage :: Text -> Text -> Text -> SyntaxItem
simpleMessage name' next marker =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegionOffset ("MyoHs" <> name') marker errorEnd Nothing "ms=e+1" ""
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHs" <> next)]

doNotationResultDiscarded :: SyntaxItem
doNotationResultDiscarded =
  simpleMessage "DoResDiscard" "Code" doResDiscardMarker

invalidImportName :: SyntaxItem
invalidImportName =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegion "MyoHsInvalidImportName" invalidImportNameMarker errorEnd Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsInvalidImportNameHead")]

invalidImportNameHead :: SyntaxItem
invalidImportNameHead =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsInvalidImportNameHead" invalidImportNameMarker
    options = ["contained", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsNames")]

moduleNameMismatch :: SyntaxItem
moduleNameMismatch =
  simpleMessage "ModuleNameMismatch" "Found" moduleNameMismatchMarker

unknownModule :: SyntaxItem
unknownModule =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegionOffset "MyoHsUnknownModule" unknownModuleMarker errorEnd Nothing "ms=e+1" ""
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsUnknownModuleHead")]

unknownModuleHead :: SyntaxItem
unknownModuleHead =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsUnknownModuleHead" unknownModuleMarker
    options = ["contained", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsModule")]

invalidQualifiedName :: SyntaxItem
invalidQualifiedName =
  simpleMessage "InvalidQualifiedName" "Code" invalidQualifiedNameMarker

runtimeError :: SyntaxItem
runtimeError =
  simpleMessage "RuntimeError" "Code" runtimeErrorMarker

ambiguousTypeVar :: SyntaxItem
ambiguousTypeVar =
  simpleMessage "AmbiguousTypeVar" "AmbiguousTypeVarVar" ambiguousTypeVarMarker

ambiguousTypeVarVar :: SyntaxItem
ambiguousTypeVarVar =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsAmbiguousTypeVarVar" "^.*$"
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsAmbiguousTypeVarMethod")]

ambiguousTypeVarMethod :: SyntaxItem
ambiguousTypeVarMethod =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsAmbiguousTypeVarMethod" "^.*$"
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsCode")]

patterns :: SyntaxItem
patterns =
  simpleMessage "NonExhaustivePatterns" "Code" patternsMarker

dataCtor :: SyntaxItem
dataCtor =
  simpleMessage "DataCtorNotInScope" "Code" dataCtorNotInScopeMarker

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync minlines=10"

hiReq :: Highlight
hiReq =
  syntaxHighlight "MyoHsReq" [("ctermfg", "2"), ("guifg", "#719e07")]

hiFound :: Highlight
hiFound =
  syntaxHighlight "MyoHsFound" [("ctermfg", "1"), ("guifg", "#dc322f")]

hiTrigger :: Highlight
hiTrigger =
  syntaxHighlight "MyoHsNoInstanceTrigger" [("ctermfg", "3")]

hiName :: Highlight
hiName =
  syntaxHighlight "MyoHsName" [("ctermfg", "5"), ("guifg", "#d33682")]

hlPath :: HiLink
hlPath =
  HiLink "MyoPath" "Directory"

hlError :: HiLink
hlError =
  HiLink "MyoHsError" "Error"

hlLineNumber :: HiLink
hlLineNumber =
  HiLink "MyoLineNumber" "Directory"

hlBang :: HiLink
hlBang =
  HiLink "MyoHsNoInstanceBang" "Error"

hlNoInstanceKw :: HiLink
hlNoInstanceKw =
  HiLink "MyoHsNoInstanceKw" "Directory"

hlNotInScope :: HiLink
hlNotInScope =
  HiLink "MyoHsNotInScopeHead" "Error"

hlModule :: HiLink
hlModule =
  HiLink "MyoHsModule" "Type"

hlDoNotationResDiscarded :: HiLink
hlDoNotationResDiscarded =
  HiLink "MyoHsDoResDiscardHead" "Error"

hlInvalidImportName :: HiLink
hlInvalidImportName =
  HiLink "MyoHsInvalidImportNameHead" "Error"

hlModuleNameMismatch :: HiLink
hlModuleNameMismatch =
  HiLink "MyoHsModuleNameMismatchHead" "Error"

hlUnknownModule :: HiLink
hlUnknownModule =
  HiLink "MyoHsUnknownModuleHead" "Error"

hlInvalidQualifiedName :: HiLink
hlInvalidQualifiedName =
  HiLink "MyoHsInvalidQualifiedNameHead" "Error"

hlAmbiguousTypeVar :: HiLink
hlAmbiguousTypeVar =
  HiLink "MyoHsAmbiguousTypeVarHead" "Error"

hlAmbiguousTypeVarVar :: HiLink
hlAmbiguousTypeVarVar =
  HiLink "MyoHsAmbiguousTypeVarVar" "MyoHsName"

hlAmbiguousTypeVarMethod :: HiLink
hlAmbiguousTypeVarMethod =
  HiLink "MyoHsAmbiguousTypeVarMethod" "MyoHsCode"

haskellSyntax :: Syntax
haskellSyntax =
  Syntax (items ++ [sync]) highlights hilinks
  where
    items =
      [
        haskellInclude, location, path, lineNumberSymbol, errorMessage, foundReq, found, req, code,
        noInstance, noInstanceHead, noInstanceBang, noInstanceKw, noInstanceTrigger, noInstanceDesc, notInScope,
        notInScopeHead, moduleImport, nameImports, moduleLine, names, name, doNotationResultDiscarded,
        invalidImportName, invalidImportNameHead, moduleNameMismatch, unknownModule, unknownModuleHead,
        invalidQualifiedName, runtimeError, ambiguousTypeVar, ambiguousTypeVarVar, ambiguousTypeVarMethod, patterns,
        dataCtor
      ]
    highlights = [hiReq, hiFound, hiTrigger, hiName]
    hilinks =
      [
        hlError, hlPath, hlLineNumber, hlBang, hlNoInstanceKw, hlNotInScope, hlModule, hlDoNotationResDiscarded,
        hlInvalidImportName, hlModuleNameMismatch, hlUnknownModule, hlInvalidQualifiedName, hlAmbiguousTypeVar,
        hlAmbiguousTypeVarVar, hlAmbiguousTypeVarMethod
      ]
