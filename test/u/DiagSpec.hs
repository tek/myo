{-# OPTIONS_GHC -F -pgmF htfpp #-}

module DiagSpec(
  htf_thisModulesTests
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Default (Default(def))
import Ribosome.Api.Buffer (currentBufferContent)
import Test.Framework

import Config (vars)
import Myo.Data.Myo (Myo)
import Myo.Diag (myoDiag)
import Myo.Test.Unit (tmuxSpecWithDef)

target :: [String]
target = [
  "Diagnostics",
  ""
  ]

diagSpec :: Myo ()
diagSpec = do
  lift $ myoDiag def
  content <- lift currentBufferContent
  liftIO $ assertEqual target content

test_diag :: IO ()
test_diag =
  vars >>= tmuxSpecWithDef diagSpec
