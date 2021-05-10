module Myo.Test.Output.PathTest where

import Hedgehog ((===))
import Path (parseAbsDir, parseAbsFile)
import Ribosome.Nvim.Api.IO (vimSetOption)
import Ribosome.Test.Run (UnitTest)
import Ribosome.Test.Unit (tempDir, tempFile)

import Myo.Output.ParseReport (findFile)
import Myo.Test.Unit (MyoTest, testDef)

base :: String
base =
  "output/resolve-path"

outputResolvePathTest :: MyoTest ()
outputResolvePathTest = do
  cwd <- parseAbsDir =<< tempDir base
  targetFile <- tempFile (base <> "/sub/dir/target")
  vimSetOption "path" "sub/"
  writeFile targetFile ""
  target <- parseAbsFile targetFile
  (target ===) =<< findFile cwd "dir/target"

test_outputResolvePath :: UnitTest
test_outputResolvePath = do
  testDef outputResolvePathTest
