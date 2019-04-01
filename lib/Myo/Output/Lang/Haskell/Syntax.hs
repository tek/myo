module Myo.Output.Lang.Haskell.Syntax where

import qualified Data.Map as Map (empty, fromList)
import Ribosome.Data.Syntax (
  HiLink(..),
  Highlight(..),
  Syntax(Syntax),
  SyntaxItem(..),
  SyntaxItemDetail(..),
  syntaxHighlight,
  syntaxMatch,
  syntaxRegion,
  syntaxVerbatim,
  )

import Myo.Output.Data.String (colMarker, lineNumber)

errorEnd :: String
errorEnd = "\\ze.*\\(" ++ lineNumber ++ "\\|" ++ colMarker ++ "\\)"

foundReqHead :: String
foundReqHead =
  "type mismatch"

noInstanceMarker :: String
noInstanceMarker =
  "\\s*!instance:"

notInScopeMarker :: String
notInScopeMarker = "Variable not in scope:"

haskellInclude :: SyntaxItem
haskellInclude =
  syntaxVerbatim "syntax include @haskell syntax/haskell.vim"

location :: SyntaxItem
location =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoLocation" ("^.*" ++ lineNumber ++ ".*$")
    options = ["skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoPath,MyoLineNumber"), ("nextgroup", "MyoHsError")]

path :: SyntaxItem
path =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoPath" ("^.*\\ze\\( " ++ lineNumber ++ ".*$\\)\\@=")
    options = ["contained"]
    params = Map.fromList [("containedin", "MyoLocation")]

lineNumberSymbol :: SyntaxItem
lineNumberSymbol =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoLineNumber" ("\\(" ++ lineNumber ++ " \\)\\@<=\\zs\\d\\+\\ze")
    options = ["contained"]
    params = Map.fromList [("containedin", "MyoLocation")]

errorMessage :: SyntaxItem
errorMessage =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegion "MyoHsError" "^" errorEnd Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoHsFoundReq,MyoHsNoInstance,MyoHsNotInScope")]

foundReq :: SyntaxItem
foundReq =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegion "MyoHsFoundReq" foundReqHead errorEnd Nothing
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
    item = syntaxMatch "MyoHsNoInstanceHead" ("\\s*" ++ noInstanceMarker ++ ".*$")
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
  item { siOptions = options }
  where
    item = syntaxMatch "MyoHsNoInstanceTrigger" ".*"
    options = ["contained"]

noInstanceDesc :: SyntaxItem
noInstanceDesc =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoHsNoInstanceDesc" ".*"
    options = ["contained", "skipnl"]
    params = Map.fromList [("contains", "@haskell"), ("nextgroup", "MyoLocation")]

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
    item = syntaxMatch "MyoHsNotInScopeHead" ("\\s*" ++ notInScopeMarker)
    options = ["contained", "skipnl"]
    params = Map.fromList [("nextgroup", "MyoHsCode")]

hiReq :: Highlight
hiReq =
  syntaxHighlight "MyoHsReq" [("ctermfg", "2"), ("guifg", "#719e07")]

hiTrigger :: Highlight
hiTrigger =
  syntaxHighlight "MyoHsNoInstanceTrigger" [("ctermfg", "3")]

hlPath :: HiLink
hlPath =
  HiLink "MyoPath" "Directory"

hlLineNumber :: HiLink
hlLineNumber =
  HiLink "MyoLineNumber" "Directory"

hlFoundReq :: HiLink
hlFoundReq =
  HiLink "MyoHsFoundReq" "Title"

hlFound :: HiLink
hlFound =
  HiLink "MyoHsFound" "Error"

hlBang :: HiLink
hlBang =
  HiLink "MyoHsNoInstanceBang" "Error"

hlKw :: HiLink
hlKw =
  HiLink "MyoHsNoInstanceKw" "Directory"

hlNotInScope :: HiLink
hlNotInScope =
  HiLink "MyoHsNotInScopeHead" "Error"

haskellSyntax :: Syntax
haskellSyntax =
  Syntax items highlights hilinks
  where
    items =
      [
        haskellInclude, location, path, lineNumberSymbol, errorMessage, foundReq, found, req, code,
        noInstance, noInstanceHead, noInstanceBang, noInstanceKw, noInstanceTrigger, noInstanceDesc, notInScope,
        notInScopeHead
      ]
    highlights = [hiReq, hiTrigger]
    hilinks = [hlPath, hlLineNumber, hlFoundReq, hlFound, hlBang, hlKw, hlNotInScope]
