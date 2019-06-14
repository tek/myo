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
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoPath" ("^.*\\ze\\( " <> lineNumber <> ".*$\\)\\@=")
    options = ["contained"]
    params = Map.fromList [("containedin", "MyoLocation")]

lineNumberSymbol :: SyntaxItem
lineNumberSymbol =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoLineNumber" ("\\(" <> lineNumber <> " \\)\\@<=\\zs\\d\\+\\ze")
    options = ["contained"]
    params = Map.fromList [("containedin", "MyoLocation")]

errorMessage :: SyntaxItem
errorMessage =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegion "MyoHsError" "^" errorEnd Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", contains)]
    contains = "MyoHsFoundReq,MyoHsNoInstance,MyoHsNotInScope,MyoHsModuleImport,MyoHsNameImports,MyoHsDoResDiscard"

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

doNotationResultDiscarded :: SyntaxItem
doNotationResultDiscarded =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegionOffset "MyoHsDoResDiscard" doResDiscardMarker errorEnd Nothing "ms=e+1" ""
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsDoResDiscardHead")]

doNotationResultDiscardedHead :: SyntaxItem
doNotationResultDiscardedHead =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsDoResDiscardHead" ("\\s*" <> doResDiscardMarker)
    options = ["contained", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsCode")]

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync match MyoHsSync grouphere MyoLocation /^$/"

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

haskellSyntax :: Syntax
haskellSyntax =
  Syntax items highlights hilinks
  where
    items =
      [
        haskellInclude, location, path, lineNumberSymbol, errorMessage, foundReq, found, req, code,
        noInstance, noInstanceHead, noInstanceBang, noInstanceKw, noInstanceTrigger, noInstanceDesc, notInScope,
        notInScopeHead, moduleImport, nameImports, moduleLine, names, name, doNotationResultDiscarded,
        doNotationResultDiscardedHead, sync
      ]
    highlights = [hiReq, hiFound, hiTrigger, hiName]
    hilinks =
      [
        hlError, hlPath, hlLineNumber, hlBang, hlNoInstanceKw, hlNotInScope, hlModule
      ]
