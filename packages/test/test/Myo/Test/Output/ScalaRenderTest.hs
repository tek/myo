module Myo.Test.Output.ScalaRenderTest where

import qualified Data.Text as Text (dropWhileEnd, lines, take)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList, zipWith)
import Polysemy.Test (UnitTest, (===))
import Ribosome (Rpc)
import Ribosome.Api (nvimCommand, nvimCommandOutput)
import Ribosome.Api.Syntax (executeSyntax)
import qualified Ribosome.Settings as Settings
import Ribosome.Syntax (Syntax (..), syntaxHighlight)
import Ribosome.Test (testError)
import Ribosome.Test.Screenshot (awaitScreenshot)

import Myo.Command.Output (compileAndRenderReport)
import Myo.Command.Parse (storeParseResult)
import qualified Myo.Output.Data.EventIndex as EventIndex (Relative (Relative))
import Myo.Output.Data.Location (Location (Location))
import Myo.Output.Data.OutputEvent (OutputEvent (OutputEvent), OutputEventMeta (OutputEventMeta))
import Myo.Output.Data.OutputEvents (OutputEvents (OutputEvents))
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine (..))
import Myo.Output.Lang.Scala.Syntax (scalaSyntax)
import qualified Myo.Settings as Settings
import Myo.Test.Embed (myoSocketTmuxTest)

loc :: Location
loc =
  Location "/path/to/file.scala" 10 Nothing

eventMetas :: Vector OutputEventMeta
eventMetas =
  Vector.fromList [
    OutputEventMeta (Just loc) 0,
    OutputEventMeta (Just loc) 0,
    OutputEventMeta (Just loc) 0,
    OutputEventMeta (Just loc) 1
    ]

reportLines :: Vector (Vector (ReportLine EventIndex.Relative))
reportLines =
  Vector.fromList [
    Vector.fromList [
      ReportLine i0 "/path/to/file.scala \57505 3",
      ReportLine i0 "expected class or object definition",
      ReportLine i0 "  \8224name",
      ReportLine i0 ""
      ],
    Vector.fromList [
      ReportLine i1 "/path/to/other_file.scala \57505 7",
      ReportLine i1 "terrible mistake",
      ReportLine i1 "  !I param: Type",
      ReportLine i1 "  Foo.bar invalid because",
      ReportLine i1 "  !I param2: Class",
      ReportLine i1 "  \8224implicitly[Class]",
      ReportLine i1 ""
      ],
    Vector.fromList [
      ReportLine i2 "/path/to/third_file.scala \57505 3",
      ReportLine i2 "type mismatch",
      ReportLine i2 "  Type[+Int <| List[String]-]",
      ReportLine i2 "  func(\8224param)",
      ReportLine i2 ""
      ],
    Vector.fromList [
      ReportLine i3 "/path/to/warning.scala \57505 3",
      ReportLine i3 "unused import"
      ]
    ]
    where
      i0 = EventIndex.Relative 0
      i1 = EventIndex.Relative 1
      i2 = EventIndex.Relative 2
      i3 = EventIndex.Relative 3

events :: OutputEvents
events =
  OutputEvents (Vector.zipWith OutputEvent eventMetas reportLines)

parsedOutput :: ParsedOutput
parsedOutput =
  ParsedOutput scalaSyntax events

syntaxTarget :: [Text]
syntaxTarget = [
  "MyoPath        xxx match /^.*\\ze\\( \57505.*$\\)\\@=/  contained",
  "MyoLineNumber  xxx match /\\(\57505 \\)\\@<=\\zs\\d\\+\\ze/  contained",
  "MyoError       xxx start=/./ end=/\\ze.*\\(\57505\\|\8224\\)/  contained contains=MyoSplain,MyoSplainFoundReq nextgroup=MyoScalaCode skipwhite skipnl",
  "MyoLocation    xxx match /^.*\57505.*$/  contains=MyoPath,MyoLineNumber nextgroup=MyoError skipwhite skipnl",
  "MyoSplain      xxx start=/\\s*!I/ end=/\\ze.*\\(\57505\\|\8224\\)/  contained contains=MyoSplainParam,MyoSplainCandidate",
  "MyoSplainFoundReq xxx match /^.*+.\\{-}<|.\\{-}-.*$/  contained contains=MyoSplainFound,MyoSplainReq",
  "MyoScalaCode   xxx match /^.*$/  contained keepend contains=@scala,MyoColMarker",
  "MyoCol         xxx match /./  contained",
  "MyoColMarker   xxx match /\8224/  contained conceal containedin=@scala nextgroup=MyoCol",
  "MyoSplainParam xxx match /\\s*!I.*$/  contained contains=MyoSplainParamMarker",
  "MyoSplainCandidate xxx match /\\S\\+\\ze invalid because/  contained",
  "MyoSplainParamMarker xxx match /!I/  contained contains=MyoSplainParamMarkerBang nextgroup=MyoSplainParamName",
  "MyoSplainParamMarkerBang xxx match /!/  contained nextgroup=MyoSplainParamMarkerI",
  "MyoSplainParamName xxx match /[^:]\\+\\ze:/  contained nextgroup=MyoSplainParamType",
  "MyoSplainParamMarkerI xxx match /I/  contained nextgroup=MyoSplainParamName skipwhite",
  "MyoSplainParamType xxx start=/./ end=/\\ze.*\\(invalid because\\|\8224\\)/  contained",
  "MyoSplainFound xxx match /\\zs+.\\{-}<\\ze/  contained contains=MyoSplainFoundReqMarker nextgroup=MyoSplainReq",
  "MyoSplainReq   xxx match /|\\zs.\\{-}-\\ze/  contained contains=MyoSplainFoundReqMarker",
  "MyoSplainFoundReqMarker xxx match /+\\|<\\|-/  contained conceal",
  "MyoPath        xxx links to Directory",
  "MyoLineNumber  xxx links to Directory",
  "MyoError       xxx links to Error", "MyoLocation    xxx cleared",
  "MyoSplain      xxx cleared",
  "MyoSplainFoundReq xxx links to Normal",
  "MyoScalaCode   xxx cleared", "MyoCol         xxx links to Search",
  "MyoColMarker   xxx cleared", "MyoSplainParam xxx cleared",
  "MyoSplainCandidate xxx links to Error",
  "MyoSplainParamMarker xxx cleared",
  "MyoSplainParamMarkerBang xxx links to Error",
  "MyoSplainParamName xxx links to Type",
  "MyoSplainParamMarkerI xxx links to Directory",
  "MyoSplainParamType xxx links to Statement",
  "MyoSplainFound xxx links to Error",
  "MyoSplainReq   xxx ctermfg=2 guifg=#719e07",
  "MyoSplainFoundReqMarker xxx cleared"
  ]

setupHighlights ::
  Member Rpc r =>
  Sem r ()
setupHighlights =
  void $ executeSyntax (Syntax [] [syntaxHighlight "Error" [("ctermfg", "1"), ("cterm", "bold")]] [])

myoSyntax ::
  Member Rpc r =>
  Sem r [Text]
myoSyntax = do
  syntax <- parse <$> nvimCommandOutput "syntax"
  hi <- parse <$> nvimCommandOutput "hi"
  pure $ syntax <> hi
  where
    parse = fmap (Text.dropWhileEnd (' ' ==)) <$> filter isMyo . Text.lines
    isMyo item = Text.take 3 item == "Myo"

test_scalaRender :: UnitTest
test_scalaRender =
  myoSocketTmuxTest do
    Settings.update Settings.outputSelectFirst True
    Settings.update Settings.outputAutoJump False
    setupHighlights
    storeParseResult "test" [parsedOutput]
    testError compileAndRenderReport
    nvimCommand "wincmd w"
    syntax <- myoSyntax
    syntaxTarget === syntax
    awaitScreenshot False "render-scala-parse-result" 0
