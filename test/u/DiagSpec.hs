{-# OPTIONS_GHC -F -pgmF htfpp #-}

module DiagSpec (htf_thisModulesTests) where

import Ribosome.Api.Buffer (currentBufferContent)
import Test.Framework

import Myo.Data.Env (Myo)
import Myo.Diag (myoDiag)
import Unit (specDef)

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

diagSpec :: Myo ()
diagSpec = do
  myoDiag
  content <- currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  specDef diagSpec
