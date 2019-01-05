{-# OPTIONS_GHC -F -pgmF htfpp #-}

module DiagSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (Default(def))
import Test.Framework
import Ribosome.Api.Buffer (currentBufferContent)
import Myo.Data.Myo (Myo)
import Myo.Test.Unit (specWithDef)
import Myo.Diag (myoDiag)
import Config (vars)

target :: [String]
target = [
  "Diagnostics",
  ""
  ]

diagSpec :: Myo ()
diagSpec = do
  myoDiag def
  content <- currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  vars >>= specWithDef diagSpec
