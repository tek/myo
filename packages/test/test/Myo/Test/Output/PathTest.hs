module Myo.Test.Output.PathTest where

import Path (parseAbsDir, parseAbsFile)
import Ribosome.Nvim.Api.IO (vimSetOption)

import Myo.Output.ParseReport (findFile)

base :: String
base =
  "output/resolve-path"

outputResolvePathTest :: Sem r ()
outputResolvePathTest = do
  cwd <- parseAbsDir =<< tempDir base
  targetFile <- tempFile (base <> "/sub/dir/target")
  vimSetOption "path" "sub/"
  writeFile targetFile ""
  target <- parseAbsFile targetFile
  (target ===) =<< findFile cwd "dir/target"

test_outputResolvePath :: UnitTest
test_outputResolvePath = do
  myoTest outputResolvePathTest
