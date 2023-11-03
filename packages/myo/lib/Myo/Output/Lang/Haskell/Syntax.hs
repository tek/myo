module Myo.Output.Lang.Haskell.Syntax where

import qualified Data.Map.Strict as Map
import Exon (exon)
import Ribosome.Syntax (
  HiLink (..),
  Highlight (..),
  Syntax (Syntax),
  SyntaxGroup (SyntaxGroup),
  SyntaxItem (..),
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
  [exon|\v\ze.*(#{lineNumber}|#{colMarker})|]

foundReqMarker :: Text
foundReqMarker =
  "type mismatch"

kindMismatchMarker :: Text
kindMismatchMarker =
  "kind mismatch"

noInstanceMarker :: Text
noInstanceMarker =
  "\\s*!instance:"

noEffectMarker :: Text
noEffectMarker =
  "\\s*!effect:"

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

dataconNotInScopeMarker :: Text
dataconNotInScopeMarker =
  "data constructor not in scope"

invalidRecordFieldMarker :: Text
invalidRecordFieldMarker =
  "Invalid or out-of-scope record field"

undeterminedRecordTypeMarker :: Text
undeterminedRecordTypeMarker =
  "Undetermined record type in field selection"

haskellInclude :: SyntaxItem
haskellInclude =
  syntaxVerbatim "syntax include @haskell syntax/haskell.vim"

location :: SyntaxItem
location =
  item { options, params }
  where
    item = syntaxMatch "MyoLocation" ("^.*" <> lineNumber <> ".*$")
    options = ["skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoPath,MyoLineNumber"), ("nextgroup", "MyoHsError")]

path :: SyntaxItem
path =
  item { options }
  where
    item = syntaxMatch "MyoPath" ("^.*\\ze\\( " <> lineNumber <> ".*$\\)\\@=")
    options = ["contained"]

lineNumberSymbol :: SyntaxItem
lineNumberSymbol =
  item { options }
  where
    item = syntaxMatch "MyoLineNumber" ("\\(" <> lineNumber <> " \\)\\@<=\\zs\\d\\+\\ze")
    options = ["contained"]

errorMessage :: SyntaxItem
errorMessage =
  item { options, params }
  where
    item = syntaxRegion "MyoHsError" "^" errorEnd Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", contains1 <> contains2 <> contains3 <> contains4)]
    contains1 =
      "MyoHsFoundReq,MyoHsNoInstance,MyoHsNotInScope,MyoHsModuleImport,MyoHsNameImports,MyoHsDoResDiscard"
    contains2 =
      ",MyoHsInvalidImportName,MyoHsModuleNameMismatch,MyoHsUnknownModule,MyoHsInvalidQualifiedName"
    contains3 =
      ",MyoHsAmbiguousTypeVar,MyoHsRuntimeError,MyoHsNonexhaustivePatterns,MyoHsDataConNotInScope,MyoHsNoEffect"
    contains4 =
      ",MyoHsInvalidRecordField,MyoHsUndeterminedRecordType"

foundReq :: SyntaxItem
foundReq =
  item { options, params }
  where
    item = syntaxRegionOffset "MyoHsFoundReq" foundReqMarker errorEnd Nothing (Just "ms=e+1") Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsFound")]

kindMismatch :: SyntaxItem
kindMismatch =
  item { options, params }
  where
    item = syntaxRegionOffset "MyoHsKindMismatch" kindMismatchMarker errorEnd Nothing (Just "ms=e+1") Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsFound")]

found :: SyntaxItem
found =
  item { options, params }
  where
    item = syntaxMatch "MyoHsFound" "^.*$"
    options = ["contained", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsReq")]

req :: SyntaxItem
req =
  item { options }
  where
    item = syntaxMatch "MyoHsReq" "^.*$"
    options = ["contained"]

code :: SyntaxItem
code =
  item { options, params }
  where
    item = syntaxMatch "MyoHsCode" ".*"
    options = ["contained", "keepend"]
    params = Map.fromList [("contains", "@haskell")]

noInstance :: SyntaxItem
noInstance =
  item { options, params }
  where
    item = syntaxRegion "MyoHsNoInstance" noInstanceMarker errorEnd Nothing
    options = ["contained"]
    params = Map.fromList [("contains", "MyoHsNoInstanceHead")]

noInstanceHead :: SyntaxItem
noInstanceHead =
  item { options, params }
  where
    item = syntaxMatch "MyoHsNoInstanceHead" ("\\s*" <> noInstanceMarker <> ".*$")
    options = ["contained", "skipnl"]
    params = Map.fromList [("contains", "MyoHsNoInstanceBang"), ("nextgroup", "MyoHsNoInstanceDesc")]

noInstanceBang :: SyntaxItem
noInstanceBang =
  item { options, params }
  where
    item = syntaxMatch "MyoHsNoInstanceBang" "!"
    options = ["contained"]
    params = Map.fromList [("nextgroup", "MyoHsNoInstanceKw")]

noInstanceKw :: SyntaxItem
noInstanceKw =
  item { options, params }
  where
    item = syntaxMatch "MyoHsNoInstanceKw" "instance\\ze:"
    options = ["contained", "skipwhite"]
    params = Map.fromList [("nextgroup", "MyoHsNoInstanceTrigger")]

noInstanceTrigger :: SyntaxItem
noInstanceTrigger =
  item { options, params }
  where
    item = syntaxMatch "MyoHsNoInstanceTrigger" ".*"
    options = ["contained", "skipnl", "keepend"]
    params = Map.fromList [("contains", "@haskell"), ("nextgroup", "MyoHsNoInstanceDesc")]

noInstanceDesc :: SyntaxItem
noInstanceDesc =
  item { options, params }
  where
    item = syntaxMatch "MyoHsNoInstanceDesc" ".*"
    options = ["contained", "skipnl", "keepend"]
    params = Map.fromList [("contains", "@haskell")]

notInScope :: SyntaxItem
notInScope =
  item { options, params }
  where
    item = syntaxRegion "MyoHsNotInScope" notInScopeMarker errorEnd Nothing
    options = ["contained"]
    params = Map.fromList [("contains", "MyoHsNotInScopeHead")]

notInScopeHead :: SyntaxItem
notInScopeHead =
  item { options, params }
  where
    item = syntaxMatch "MyoHsNotInScopeHead" ("\\s*" <> notInScopeMarker)
    options = ["contained", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsCode")]

moduleImport :: SyntaxItem
moduleImport =
  item { options, params }
  where
    item = syntaxRegionOffset "MyoHsModuleImport" moduleImportMarker errorEnd Nothing (Just "ms=e+1") Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsModule")]

nameImports :: SyntaxItem
nameImports =
  item { options, params }
  where
    item = syntaxRegionOffset "MyoHsNameImports" nameImportsMarker errorEnd Nothing (Just "ms=e+1") Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsNames")]

names :: SyntaxItem
names =
  item { options, params }
  where
    item = syntaxMatch "MyoHsNames" "^.*$"
    options = ["contained", "skipnl"]
    params = Map.fromList [("contains", "MyoHsName"), ("nextgroup", "MyoHsModule")]

name :: SyntaxItem
name =
  item { options }
  where
    item = syntaxMatch "MyoHsName" "\\w\\+"
    options = ["contained"]

moduleLine :: SyntaxItem
moduleLine =
  item { options }
  where
    item = syntaxMatch "MyoHsModule" "^.*$"
    options = ["contained", "skipnl"]

simpleMessage :: Text -> Text -> Text -> SyntaxItem
simpleMessage name' next marker =
  item { options, params }
  where
    item = syntaxRegionOffset (SyntaxGroup [exon|MyoHs#{name'}|]) marker errorEnd Nothing (Just "ms=e+1") Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHs" <> next)]

doNotationResultDiscarded :: SyntaxItem
doNotationResultDiscarded =
  simpleMessage "DoResDiscard" "Code" doResDiscardMarker

invalidImportName :: SyntaxItem
invalidImportName =
  item { options, params }
  where
    item = syntaxRegion "MyoHsInvalidImportName" invalidImportNameMarker errorEnd Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsInvalidImportNameHead")]

invalidImportNameHead :: SyntaxItem
invalidImportNameHead =
  item { options, params }
  where
    item = syntaxMatch "MyoHsInvalidImportNameHead" invalidImportNameMarker
    options = ["contained", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsNames")]

moduleNameMismatch :: SyntaxItem
moduleNameMismatch =
  simpleMessage "ModuleNameMismatch" "Found" moduleNameMismatchMarker

unknownModule :: SyntaxItem
unknownModule =
  item { options, params }
  where
    item = syntaxRegionOffset "MyoHsUnknownModule" unknownModuleMarker errorEnd Nothing (Just "ms=e+1") Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsUnknownModuleHead")]

unknownModuleHead :: SyntaxItem
unknownModuleHead =
  item { options, params }
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
  item { options, params }
  where
    item = syntaxMatch "MyoHsAmbiguousTypeVarVar" "^.*$"
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsAmbiguousTypeVarMethod")]

ambiguousTypeVarMethod :: SyntaxItem
ambiguousTypeVarMethod =
  item { options, params }
  where
    item = syntaxMatch "MyoHsAmbiguousTypeVarMethod" "^.*$"
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsCode")]

patterns :: SyntaxItem
patterns =
  simpleMessage "NonExhaustivePatterns" "Code" patternsMarker

datacon :: SyntaxItem
datacon =
  simpleMessage "DataConNotInScope" "Code" dataconNotInScopeMarker

invalidRecordField :: SyntaxItem
invalidRecordField =
  simpleMessage "InvalidRecordField" "RecordField" invalidRecordFieldMarker

undeterminedRecordType :: SyntaxItem
undeterminedRecordType =
  simpleMessage "UndeterminedRecordType" "RecordField" undeterminedRecordTypeMarker

recordField :: SyntaxItem
recordField =
  item { options, params }
  where
    item = syntaxMatch "RecordField" "^.*$"
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("nextgroup", "RecordFieldType")]

recordFieldType :: SyntaxItem
recordFieldType =
  item { options }
  where
    item = syntaxMatch "RecordFieldType" "^.*$"
    options = ["contained", "skipwhite", "skipnl"]

noEffect :: SyntaxItem
noEffect =
  item { options, params }
  where
    item = syntaxRegion "MyoHsNoEffect" noEffectMarker errorEnd Nothing
    options = ["contained"]
    params = Map.fromList [("contains", "MyoHsNoEffectHead")]

noEffectHead :: SyntaxItem
noEffectHead =
  item { options, params }
  where
    item = syntaxMatch "MyoHsNoEffectHead" ("\\s*" <> noEffectMarker <> ".*$")
    options = ["contained", "skipnl"]
    params = Map.fromList [("contains", "MyoHsNoEffectBang")]

noEffectBang :: SyntaxItem
noEffectBang =
  item { options, params }
  where
    item = syntaxMatch "MyoHsNoEffectBang" "!"
    options = ["contained"]
    params = Map.fromList [("nextgroup", "MyoHsNoEffectKw")]

noEffectKw :: SyntaxItem
noEffectKw =
  item { options, params }
  where
    item = syntaxMatch "MyoHsNoEffectKw" "effect\\ze:"
    options = ["contained", "skipwhite"]
    params = Map.fromList [("nextgroup", "MyoHsNoEffectEffect")]

noEffectEffect :: SyntaxItem
noEffectEffect =
  item { options, params }
  where
    item = syntaxMatch "MyoHsNoEffectEffect" ".*$"
    options = ["contained", "skipnl"]
    params = Map.empty

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

hlRecordField :: HiLink
hlRecordField =
  HiLink "MyoHsRecordField" "MyoHsCode"

hlRecordFieldType :: HiLink
hlRecordFieldType =
  HiLink "MyoHsRecordFieldType" "Type"

hlEffectBang :: HiLink
hlEffectBang =
  HiLink "MyoHsNoEffectBang" "Error"

hlNoEffectKw :: HiLink
hlNoEffectKw =
  HiLink "MyoHsNoEffectKw" "Directory"

hlEffect :: HiLink
hlEffect =
  HiLink "MyoHsNoEffectEffect" "Type"

haskellSyntax :: Syntax
haskellSyntax =
  Syntax (items ++ [sync]) highlights hilinks
  where
    items =
      [
        haskellInclude, location, path, lineNumberSymbol, errorMessage, foundReq, kindMismatch, found, req, code,
        noInstance, noInstanceHead, noInstanceBang, noInstanceKw, noInstanceTrigger, noInstanceDesc, notInScope,
        notInScopeHead, moduleImport, nameImports, moduleLine, names, name, doNotationResultDiscarded,
        invalidImportName, invalidImportNameHead, moduleNameMismatch, unknownModule, unknownModuleHead,
        invalidQualifiedName, runtimeError, ambiguousTypeVar, ambiguousTypeVarVar, ambiguousTypeVarMethod, patterns,
        datacon, invalidRecordField, undeterminedRecordType, recordField, recordFieldType, noEffect, noEffectBang,
        noEffectKw, noEffectHead, noEffectEffect
      ]
    highlights = [hiReq, hiFound, hiTrigger, hiName]
    hilinks =
      [
        hlError, hlPath, hlLineNumber, hlBang, hlNoInstanceKw, hlNotInScope, hlModule, hlDoNotationResDiscarded,
        hlInvalidImportName, hlModuleNameMismatch, hlUnknownModule, hlInvalidQualifiedName, hlAmbiguousTypeVar,
        hlAmbiguousTypeVarVar, hlAmbiguousTypeVarMethod, hlRecordField, hlRecordFieldType, hlEffect, hlEffectBang,
        hlNoEffectKw
      ]
