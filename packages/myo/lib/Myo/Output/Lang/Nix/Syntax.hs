module Myo.Output.Lang.Nix.Syntax where

import qualified Data.Map.Strict as Map
import Exon (exon)
import Ribosome.Syntax (HiLink (..), Syntax (Syntax), SyntaxItem (..), syntaxMatch, syntaxVerbatim)

import Myo.Output.Data.String (colMarker, lineNumber)

errorEnd :: Text
errorEnd =
  [exon|\v\ze.*(#{lineNumber}|#{colMarker})|]

location :: SyntaxItem
location =
  item { options, params }
  where
    item = syntaxMatch "MyoLocation" ("^.*" <> lineNumber <> ".*$")
    options = ["skipwhite", "skipnl"]
    params = Map.fromList [("contains", "MyoPath,MyoLineNumber"), ("nextgroup", "MyoNixError")]

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
    item = syntaxMatch "MyoNixError" ".*"
    options = ["contained", "skipwhite", "skipnl"]
    params = []

sync :: SyntaxItem
sync =
  syntaxVerbatim "syntax sync minlines=10"

hlPath :: HiLink
hlPath =
  HiLink "MyoPath" "Directory"

hlError :: HiLink
hlError =
  HiLink "MyoNixError" "Error"

hlLineNumber :: HiLink
hlLineNumber =
  HiLink "MyoLineNumber" "Directory"

nixSyntax :: Syntax
nixSyntax =
  Syntax (items ++ [sync]) highlights hilinks
  where
    items =
      [
        location, path, lineNumberSymbol, errorMessage
      ]
    highlights = []
    hilinks =
      [
        hlError, hlPath, hlLineNumber
      ]
