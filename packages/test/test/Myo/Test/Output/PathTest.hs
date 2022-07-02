module Myo.Test.Output.PathTest where

import qualified Data.Text.IO as Text
import Path (Dir, Path, Rel, reldir, relfile, toFilePath, (</>))
import qualified Polysemy.Test as Test
import Polysemy.Test (UnitTest, assertEq)
import Ribosome.Api (nvimSetOption)
import Ribosome.Test (testError)

import Myo.Output.ParseReport (findFile)
import Myo.Test.Embed (myoTest)

base :: Path Rel Dir
base =
  [reldir|output/resolve-path|]

test_outputResolvePath :: UnitTest
test_outputResolvePath = do
  myoTest do
    cwd <- Test.tempDir base
    targetFile <- Test.tempFile [] (base </> [relfile|sub/dir/target|])
    nvimSetOption "path" ("sub/" :: Text)
    embed (Text.writeFile (toFilePath targetFile) "")
    assertEq targetFile =<< testError (findFile cwd "dir/target")
