module Myo.Test.Output.ParseScalaTest where

import qualified Data.Text as Text (unlines)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Hedgehog (evalEither, (===))
import Ribosome.Test.Run (UnitTest)

import Myo.Command.Parse (parseWith)
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.ParseReport as ParseReport (_lines)
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import qualified Myo.Output.Data.ReportLine as ReportLine (_text)
import Myo.Output.Data.String (colMarker)
import Myo.Output.Lang.Scala.Parser hiding (parseScala)
import Myo.Output.Lang.Scala.Syntax (foundMarker, reqMarker, separatorMarker)
import Myo.Output.ParseReport (compileReport)

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

parseScala :: IO (Either OutputError ParsedOutput)
parseScala =
  runExceptT $ parseWith scalaOutputParser scalaOutput

test_parseScala :: UnitTest
test_parseScala = do
  outputE <- liftIO parseScala
  ParsedOutput _ events <- evalEither outputE
  target === (ReportLine._text <$> ParseReport._lines (compileReport 0 events))
