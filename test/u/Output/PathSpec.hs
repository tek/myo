{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Output.PathSpec (htf_thisModulesTests) where

import Ribosome.Nvim.Api.IO (bufferSetOption, vimGetCurrentBuffer)
import Path (parseAbsDir, parseAbsFile)
import Ribosome.Test.Unit (tempDir, tempFile)
import Ribosome.Api.Option (optionCat)
import System.FilePath ((</>))
import Test.Framework

import Myo.Data.Env (Myo)
import Myo.Output.ParseReport (findFile)
import Unit (specDef)

base :: String
base =
  "output/resolve-path"

outputResolvePathSpec :: Myo ()
outputResolvePathSpec = do
  cwd <- parseAbsDir =<< tempDir base
  targetFile <- tempFile (base <> "/sub/dir/target")
  buf <- vimGetCurrentBuffer
  bufferSetOption buf "path" "sub"
  writeFile targetFile ""
  target <- parseAbsFile targetFile
  gassertEqual target =<< findFile cwd "dir/target"

test_outputResolvePath :: IO ()
test_outputResolvePath = do
  specDef outputResolvePathSpec
