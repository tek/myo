{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ProcSpec (htf_thisModulesTests) where

import System.Posix.Process (getProcessID)
import Test.Framework

import Myo.System.Proc (childPids, ppids)

test_ppid :: IO ()
test_ppid = do
  pid <- getProcessID
  pps <- ppids (fromIntegral pid)
  assertBool (length pps > 2)

test_childPids :: IO ()
test_childPids = do
  cps <- childPids 1
  print cps
