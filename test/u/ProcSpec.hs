{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ProcSpec(
  htf_thisModulesTests,
) where

import Control.Monad.IO.Class (liftIO)
import System.Posix.Process (getProcessID)
import Test.Framework

import Config (vars)
import Myo.Data.Myo (Myo)
import Myo.System.Proc (ppids)
import Myo.Test.Unit (specWithDef)

test_ppid :: IO ()
test_ppid = do
  pid <- getProcessID
  pps <- ppids (fromIntegral pid)
  liftIO $ print pps
