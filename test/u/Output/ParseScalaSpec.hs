{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.ParseScalaSpec (htf_thisModulesTests) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as Text (unlines)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Test.Framework

import Myo.Command.Parse (parseWith)
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.ParseReport as ParseReport (_lines)
import Myo.Output.Data.ParsedOutput (ParsedOutput(ParsedOutput))
import qualified Myo.Output.Data.ReportLine as ReportLine (_text)
import Myo.Output.Data.String (colMarker)
import Myo.Output.Lang.Scala.Parser hiding (parseScala)
import Myo.Output.Lang.Scala.Syntax (foundMarker, reqMarker, separatorMarker)

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
    "[error] /path/to/third_file.scala:3:10: type mismatch",
    "[error]   Type[Int | List[String]]",
    "[error]     func(param)",
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

test_parseScala :: IO ()
test_parseScala = do
  outputE <- parseScala
  ParsedOutput _ cons <- assertRight outputE
  let
    report = cons 0
    lines' = ReportLine._text <$> ParseReport._lines report
  assertEqual target lines'
