{-# OPTIONS_GHC -F -pgmF htfpp #-}

module DiagSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Ribosome.Api.Buffer (currentBufferContent)
import Test.Framework

import Config (vars)
import Myo.Data.Env (MyoN)
import Myo.Diag (myoDiag)
import Myo.Test.Unit (tmuxSpecWithDef)

target :: [String]
target = [
  "Diagnostics",
  ""
  ]

diagSpec :: MyoN ()
diagSpec = do
  myoDiag
  content <- lift currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  vars >>= tmuxSpecWithDef diagSpec
