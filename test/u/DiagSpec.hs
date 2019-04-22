{-# OPTIONS_GHC -F -pgmF htfpp #-}

module DiagSpec (htf_thisModulesTests) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Ribosome.Api.Buffer (currentBufferContent)
import Test.Framework

import Myo.Data.Env (MyoN)
import Myo.Diag (myoDiag)
import Unit (tmuxSpecDef)

target :: [Text]
target = [
  "# Diagnostics",
  "",
  "## Commands",
  "",
  "",
  "## Ui",
  "",
  "",
  "## Errors"
  ]

diagSpec :: MyoN ()
diagSpec = do
  myoDiag
  content <- lift currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  tmuxSpecDef diagSpec
