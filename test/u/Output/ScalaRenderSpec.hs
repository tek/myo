{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.ScalaRenderSpec (htf_thisModulesTests) where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Data.Functor (void)
import Data.Functor.Syntax ((<$$>))
import qualified Data.Text as Text (dropWhileEnd, lines, take)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
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
import qualified Myo.Output.Data.EventIndex as EventIndex (Relative(Relative))
import Myo.Output.Data.Location (Location(Location))
import Myo.Output.Data.OutputEvent (OutputEvent(OutputEvent))
import Myo.Output.Data.ParseReport (ParseReport(ParseReport))
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import Myo.Output.Data.ReportLine (ReportLine(..))
import Myo.Output.Lang.Scala.Syntax (scalaSyntax)
import Unit (tmuxSpec)

loc :: Location
loc =
  Location "/path/to/file.scala" 10 Nothing

events :: Vector OutputEvent
events =
  Vector.fromList [OutputEvent (Just loc) 0, OutputEvent (Just loc) 1, OutputEvent (Just loc) 1]

reportLines :: Vector (ReportLine EventIndex.Relative)
reportLines =
  Vector.fromList [
    ReportLine i0 "/path/to/file.scala \57505 3",
    ReportLine i0 "expected class or object definition",
    ReportLine i0 "  \8224name",
    ReportLine i0 "",
    ReportLine i1 "/path/to/other_file.scala \57505 7",
    ReportLine i1 "terrible mistake",
    ReportLine i1 "  !I param: Type",
    ReportLine i1 "  Foo.bar invalid because",
    ReportLine i1 "  !I param2: Class",
    ReportLine i1 "  \8224implicitly[Class]",
    ReportLine i1 "",
    ReportLine i2 "/path/to/third_file.scala \57505 3",
    ReportLine i2 "type mismatch",
    ReportLine i2 "  Type[+Int <| List[String]-]",
    ReportLine i2 "  func(\8224param)",
    ReportLine i2 ""
    ]
    where
      i0 = EventIndex.Relative 0
      i1 = EventIndex.Relative 1
      i2 = EventIndex.Relative 2

parsedOutput :: ParsedOutput
parsedOutput =
  ParsedOutput scalaSyntax (ParseReport events reportLines)

syntaxTarget :: [Text]
syntaxTarget = [
  "MyoPath        xxx match /^.*\\ze\\( \57505.*$\\)\\@=/  contained  links to Directory",
  "MyoLineNumber  xxx match /\\(\57505 \\)\\@<=\\zs\\d\\+\\ze/  contained  links to Directory",
  "MyoError       xxx start=/./ end=/\\ze.*\\(\57505\\|\8224\\)/  contained contains=MyoSplain,MyoSplainFoundReq nextgroup=MyoScalaCode skipwhite skipnl  links to Error",
  "MyoLocation    xxx match /^.*\57505.*$/  contains=MyoPath,MyoLineNumber nextgroup=MyoError skipwhite skipnl",
  "MyoSplain      xxx start=/\\s*!I/ end=/\\ze.*\\(\57505\\|\8224\\)/  contained contains=MyoSplainParam,MyoSplainCandidate",
  "MyoSplainFoundReq xxx match /^.*+.\\{-}<|.\\{-}-.*$/  contained contains=MyoSplainFound,MyoSplainReq  links to Normal",
  "MyoScalaCode   xxx match /^.*$/  contained keepend contains=@scala,MyoColMarker",
  "MyoCol         xxx match /./  contained  links to Search",
  "MyoColMarker   xxx match /\8224/  contained conceal containedin=@scala nextgroup=MyoCol",
  "MyoSplainParam xxx match /\\s*!I.*$/  contained contains=MyoSplainParamMarker",
  "MyoSplainCandidate xxx match /\\S\\+\\ze invalid because/  contained  links to Error",
  "MyoSplainParamMarker xxx match /!I/  contained contains=MyoSplainParamMarkerBang nextgroup=MyoSplainParamName",
  "MyoSplainParamMarkerBang xxx match /!/  contained nextgroup=MyoSplainParamMarkerI  links to Error",
  "MyoSplainParamName xxx match /[^:]\\+\\ze:/  contained nextgroup=MyoSplainParamType  links to Type",
  "MyoSplainParamMarkerI xxx match /I/  contained nextgroup=MyoSplainParamName skipwhite  links to Directory",
  "MyoSplainParamType xxx start=/./ end=/\\ze.*\\(invalid because\\|\8224\\)/  contained  links to Statement",
  "MyoSplainFound xxx match /\\zs+.\\{-}<\\ze/  contained contains=MyoSplainFoundReqMarker nextgroup=MyoSplainReq  links to Error",
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
  MonadDeepError e DecodeError m =>
  NvimE e m =>
  m ()
setupHighlights =
  void $ executeSyntax (Syntax [] [syntaxHighlight "Error" [("ctermfg", "1"), ("cterm", "bold")]] [])

myoSyntax :: NvimE e m => m [Text]
myoSyntax = do
  syntax <- parse <$> vimCommandOutput "syntax"
  hi <- parse <$> vimCommandOutput "hi"
  return $ syntax <> hi
  where
    parse = Text.dropWhileEnd (' ' ==) <$$> filter isMyo . Text.lines
    isMyo item = Text.take 3 item == "Myo"

scalaRenderSpec :: Myo ()
scalaRenderSpec = do
  initialize''
  setupHighlights
  renderParseResult (Ident.Str "test") [parsedOutput]
  vimCommand "wincmd w"
  syntax <- myoSyntax
  gassertEqual syntaxTarget syntax
  assertScreenshot "render-scala-parse-result" False 0

test_scalaRender :: IO ()
test_scalaRender =
  tmuxSpec (svar outputSelectFirst True . svar outputAutoJump False) scalaRenderSpec
