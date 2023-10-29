module Myo.Test.Output.ParseNixTest where

import qualified Data.Text as Text (unlines)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Polysemy.Test (TestError, UnitTest, (===))
import Ribosome.Test (testError)

import Myo.Output.Data.OutputParser (runOutputParser)
import qualified Myo.Output.Data.ParseReport as ParseReport
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import qualified Myo.Output.Data.ReportLine as ReportLine
import Myo.Output.Lang.Nix.Parser (nixOutputParser)
import Myo.Output.ParseReport (compileReport)
import Myo.Test.Embed (myoTest)

haskellOutput :: Text
haskellOutput =
  Text.unlines [
    "leading crap",
    "error: value is a function while a list was expected",
    "",
    "       at /nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-source/test/fixtures/file.nix:23:5:",
    "",
    "          100|   funcname =",
    "          101|     listToAttrs (n: nameValuePair name (cons n)) things;",
    "             |     ^",
    "          102|",
    "(use '--show-trace' to show detailed location information)",
    ""
  ]

target :: Vector Text
target =
  Vector.fromList [
    "test/fixtures/file.nix \57505 23",
    "value is a function while a list was expected",
    ""
  ]

parseNix ::
  Members [Error TestError, Log, Embed IO] r =>
  Sem r ParsedOutput
parseNix =
  testError (runOutputParser nixOutputParser haskellOutput)

test_parseNixErrors :: UnitTest
test_parseNixErrors =
  myoTest do
    ParsedOutput _ events <- parseNix
    target === ((.text) <$> (compileReport 1 events).lines)
