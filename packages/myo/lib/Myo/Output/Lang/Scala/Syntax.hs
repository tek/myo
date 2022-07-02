module Myo.Output.Lang.Scala.Syntax where

import Ribosome.Syntax (
  HiLink (..),
  Highlight (..),
  Syntax (Syntax),
  SyntaxItem (..),
  syntaxHighlight,
  syntaxMatch,
  syntaxRegion,
  syntaxVerbatim,
  )

import Myo.Output.Data.String (colMarker, lineNumber)

foundReqSeparator :: Text
foundReqSeparator =
  "|"

foundMarker :: Text
foundMarker =
  "+"

reqMarker :: Text
reqMarker =
  "-"

separatorMarker :: Text
separatorMarker =
  "<"

errorEnd :: Text
errorEnd = "\\ze.*\\(" <> lineNumber <> "\\|" <> colMarker <> "\\)"

foundreqSeparator :: Text
foundreqSeparator =
  "|"

noInstanceMarker :: Text
noInstanceMarker =
  "\\s*!I"

scalaInclude :: SyntaxItem
scalaInclude =
  syntaxVerbatim "syntax include @scala syntax/scala.vim"

location :: SyntaxItem
location =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoLocation" ("^.*" <> lineNumber <> ".*$")
    options = ["skipwhite", "skipnl"]
    params = [("contains", "MyoPath,MyoLineNumber"), ("nextgroup", "MyoError")]

path :: SyntaxItem
path =
  item { siOptions = options }
  where
    item = syntaxMatch "MyoPath" ("^.*\\ze\\( " <> lineNumber <> ".*$\\)\\@=")
    options = ["contained"]

lineNumberItem :: SyntaxItem
lineNumberItem =
  item { siOptions = options }
  where
    item = syntaxMatch "MyoLineNumber" ("\\(" <> lineNumber <> " \\)\\@<=\\zs\\d\\+\\ze")
    options = ["contained"]

errorMessage :: SyntaxItem
errorMessage =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegion "MyoError" "." errorEnd Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = [("contains", "MyoSplain,MyoSplainFoundReq"), ("nextgroup", "MyoScalaCode")]

colMarkerConceal :: SyntaxItem
colMarkerConceal =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoColMarker" colMarker
    options = ["conceal", "contained"]
    params = [("nextgroup", "MyoCol"), ("containedin", "@scala")]

col :: SyntaxItem
col =
  item { siOptions = options }
  where
    item = syntaxMatch "MyoCol" "."
    options = ["contained"]

code :: SyntaxItem
code =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoScalaCode" "^.*$"
    options = ["contained", "keepend"]
    params = [("contains", "@scala,MyoColMarker")]

splain :: SyntaxItem
splain =
  item { siOptions = options, siParams = params }
  where
    item = syntaxRegion "MyoSplain" "\\s*!I" errorEnd Nothing
    options = ["contained", "skipwhite", "skipnl"]
    params = [("contains", "MyoSplainParam,MyoSplainCandidate")]

splainParam :: SyntaxItem
splainParam =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoSplainParam" (noInstanceMarker <> ".*$")
    options = ["contained"]
    params = [("contains", "MyoSplainParamMarker")]

splainParamMarker :: SyntaxItem
splainParamMarker =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoSplainParamMarker" "!I"
    options = ["contained"]
    params = [("contains", "MyoSplainParamMarkerBang"), ("nextgroup", "MyoSplainParamName")]

splainParamMarkerBang :: SyntaxItem
splainParamMarkerBang =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoSplainParamMarkerBang" "!"
    options = ["contained"]
    params = [("nextgroup", "MyoSplainParamMarkerI")]

splainParamI :: SyntaxItem
splainParamI =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoSplainParamMarkerI" "I"
    options = ["contained", "skipwhite"]
    params = [("nextgroup", "MyoSplainParamName")]

splainParamName :: SyntaxItem
splainParamName =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoSplainParamName" "[^:]\\+\\ze:"
    options = ["contained"]
    params = [("nextgroup", "MyoSplainParamType")]

splainParamType :: SyntaxItem
splainParamType =
  item { siOptions = options }
  where
    item = syntaxRegion "MyoSplainParamType" "." ("\\ze.*\\(invalid because\\|" <> colMarker <> "\\)") Nothing
    options = ["contained"]

splainCandidate :: SyntaxItem
splainCandidate =
  item { siOptions = options }
  where
    item = syntaxMatch "MyoSplainCandidate" "\\S\\+\\ze invalid because"
    options = ["contained"]

splainFoundReq :: SyntaxItem
splainFoundReq =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoSplainFoundReq" rex
    rex = "^.*" <> foundMarker <> ".\\{-}" <> separatorMarker <> foundreqSeparator <> ".\\{-}" <> reqMarker <> ".*$"
    options = ["contained"]
    params = [("contains", "MyoSplainFound,MyoSplainReq")]

splainFound :: SyntaxItem
splainFound =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoSplainFound" rex
    rex = "\\zs" <> foundMarker <> ".\\{-}" <> separatorMarker <> "\\ze"
    options = ["contained"]
    params = [("contains", "MyoSplainFoundReqMarker"), ("nextgroup", "MyoSplainReq")]

splainReq :: SyntaxItem
splainReq =
  item { siOptions = options, siParams = params }
  where
    item = syntaxMatch "MyoSplainReq" rex
    rex = foundreqSeparator <> "\\zs.\\{-}" <> reqMarker <> "\\ze"
    options = ["contained"]
    params = [("contains", "MyoSplainFoundReqMarker")]

splainFoundReqMarker :: SyntaxItem
splainFoundReqMarker =
  item { siOptions = options }
  where
    item = syntaxMatch "MyoSplainFoundReqMarker" rex
    rex = foundMarker <> "\\|" <> separatorMarker <> "\\|" <> reqMarker
    options = ["conceal", "contained"]

hiReq :: Highlight
hiReq =
  syntaxHighlight "MyoSplainReq" [("ctermfg", "2"), ("guifg", "#719e07")]

hlError :: HiLink
hlError =
  HiLink "MyoError" "Error"

hlPath :: HiLink
hlPath =
  HiLink "MyoPath" "Directory"

hlLineNumber :: HiLink
hlLineNumber =
  HiLink "MyoLineNumber" "Directory"

hlCol :: HiLink
hlCol =
  HiLink "MyoCol" "Search"

hlSplainParamMarkerBang :: HiLink
hlSplainParamMarkerBang =
  HiLink "MyoSplainParamMarkerBang" "Error"

hlSplainParamMarkerI :: HiLink
hlSplainParamMarkerI =
  HiLink "MyoSplainParamMarkerI" "Directory"

hlSplainParamName :: HiLink
hlSplainParamName =
  HiLink "MyoSplainParamName" "Type"

hlSplainParamType :: HiLink
hlSplainParamType =
  HiLink "MyoSplainParamType" "Statement"

hlSplainCandidate :: HiLink
hlSplainCandidate =
  HiLink "MyoSplainCandidate" "Error"

hlSplainFoundReq :: HiLink
hlSplainFoundReq =
  HiLink "MyoSplainFoundReq" "Normal"

hlSplainFound :: HiLink
hlSplainFound =
  HiLink "MyoSplainFound" "Error"

scalaSyntax :: Syntax
scalaSyntax =
  Syntax items highlights hilinks
  where
    items = [
      scalaInclude,
      location,
      path,
      lineNumberItem,
      errorMessage,
      colMarkerConceal,
      col,
      code,
      splain,
      splainParam,
      splainParamMarker,
      splainParamMarkerBang,
      splainParamI,
      splainParamName,
      splainParamType,
      splainCandidate,
      splainFoundReq,
      splainFound,
      splainReq,
      splainFoundReqMarker
      ]
    highlights = [hiReq]
    hilinks = [
      hlError,
      hlPath,
      hlLineNumber,
      hlCol,
      hlSplainParamMarkerBang,
      hlSplainParamMarkerI,
      hlSplainParamName,
      hlSplainParamType,
      hlSplainCandidate,
      hlSplainFoundReq,
      hlSplainFound
      ]
