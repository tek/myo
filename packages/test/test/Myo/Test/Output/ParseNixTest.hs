module Myo.Test.Output.ParseNixTest where

import qualified Data.Text as Text (unlines)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)

import Myo.Command.Parse (parseWith)
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.ParseReport as ParseReport (_lines)
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import qualified Myo.Output.Data.ReportLine as ReportLine (_text)
import Myo.Output.Lang.Nix.Parser (nixOutputParser)
import Myo.Output.ParseReport (compileReport)

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

parseNix :: IO (Either OutputError ParsedOutput)
parseNix =
  runExceptT (parseWith nixOutputParser haskellOutput)

test_parseNixErrors :: UnitTest
test_parseNixErrors = do
  outputE <- liftIO parseNix
  ParsedOutput _ events <- evalEither outputE
  target === (ReportLine._text <$> ParseReport._lines (compileReport 1 events))
