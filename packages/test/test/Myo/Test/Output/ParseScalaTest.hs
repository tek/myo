module Myo.Test.Output.ParseScalaTest where

import qualified Data.Text as Text (unlines)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Polysemy.Test (TestError, UnitTest, (===))
import Ribosome.Test.Error (testError)

import Myo.Output.Data.OutputParser (runOutputParser)
import qualified Myo.Output.Data.ParseReport as ParseReport
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import qualified Myo.Output.Data.ReportLine as ReportLine
import Myo.Output.Data.String (colMarker)
import Myo.Output.Lang.Scala.Parser (scalaOutputParser)
import Myo.Output.Lang.Scala.Syntax (foundMarker, reqMarker, separatorMarker)
import Myo.Output.ParseReport (compileReport)
import Myo.Test.Embed (myoTest)

scalaOutput :: Text
scalaOutput =
  Text.unlines [
    "leading crap",
    "[error] /path/to/file.scala:3:1: expected class or object definition",
    "[error] name",
    "[error] ^",
    "[error] /path/to/other_file.scala:7:3: terrible mistake",
    "[error] !I param: Type",
    "[error] Foo.bar invalid because",
    "[error] !I param2: Class",
    "[error]   implicitly[Class]",
    "[error]   ^",
    "",
    "  | => core / Compile / compileIncremental 0s",
    "[error] /path/to/third_file.scala:3:10: type mismatch",
    "",
    "",
    "",
    "",
    "  | => core / Compile / compileIncremental 0s",
    "[error]   Type[Int | List[String]]",
    "",
    "  | => core / Compile / compileIncremental 0s",
    "[error]     func(param)",
    "",
    "  | => core / Compile / compileIncremental 0s",
    "[error]          ^",
    "[error] one error found",
    ""
    ]

target :: Vector Text
target = Vector.fromList [
  "/path/to/file.scala \57505 3",
  "expected class or object definition",
  "  " <> colMarker <> "name",
  "",
  "/path/to/other_file.scala \57505 7",
  "terrible mistake",
  "  !I param: Type",
  "  Foo.bar invalid because",
  "  !I param2: Class",
  "  †implicitly[Class]",
  "",
  "/path/to/third_file.scala \57505 3",
  "type mismatch",
  "  Type[" <> foundMarker <> "Int " <> separatorMarker <> "| List[String]" <> reqMarker <> "]",
  "  func(†param)",
  ""
  ]

parseScala ::
  Member (Error TestError) r =>
  Sem r ParsedOutput
parseScala =
  testError (runOutputParser scalaOutputParser scalaOutput)

test_parseScala :: UnitTest
test_parseScala =
  myoTest do
    ParsedOutput _ events <- parseScala
    target === ((.text) <$> (compileReport 0 events).lines)
