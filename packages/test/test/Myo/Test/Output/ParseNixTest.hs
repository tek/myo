module Myo.Test.Output.ParseNixTest where

import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Exon (exon)
import Polysemy.Test (TestError, UnitTest, (===))
import Ribosome (Rpc, RpcError)
import Ribosome.Test (testError)

import Myo.Output.Data.OutputParser (runOutputParser)
import qualified Myo.Output.Data.ParseReport as ParseReport
import Myo.Output.Data.ParsedOutput (ParsedOutput (ParsedOutput))
import qualified Myo.Output.Data.ReportLine as ReportLine
import Myo.Output.Lang.Nix.Parser (nixOutputParser)
import Myo.Output.ParseReport (compileReport)
import Myo.Test.Embed (myoTest)

nixOutput :: Text
nixOutput =
  [exon|leading crap
error:
       … while calling the 'getAttr' builtin

         at /builtin/derivation.nix:19:19: (source not available)

       (stack trace truncated; use '--show-trace' to show the full trace)

       error: value is a function while a list was expected

       at /nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-source/test/fixtures/file.nix:23:5:

          100|   funcname =
          101|     listToAttrs (n: nameValuePair name (cons n)) things;
             |     ^
          102|
|]

target :: Vector Text
target =
  Vector.fromList [
    "test/fixtures/file.nix \57505 23",
    "value is a function while a list was expected",
    ""
  ]

parseNix ::
  Members [Error TestError, Rpc !! RpcError, Embed IO] r =>
  Sem r ParsedOutput
parseNix =
  testError (runOutputParser nixOutputParser nixOutput)

test_parseNixErrors :: UnitTest
test_parseNixErrors =
  myoTest do
    ParsedOutput _ events <- parseNix
    target === ((.text) <$> (compileReport 1 events).lines)
