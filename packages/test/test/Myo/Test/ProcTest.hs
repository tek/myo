module Myo.Test.ProcTest where

import Hedgehog ((/==), (===))
import Ribosome.Test.Run (UnitTest)
import System.Posix.Process (getProcessID)

import Myo.Command.Data.Pid (Pid(Pid))
import Myo.System.Proc (childPids, ppids)

test_ppid :: UnitTest
test_ppid = do
  pid <- liftIO getProcessID
  pps <- ppids (Pid (fromIntegral pid))
  True === (length pps > 2)

test_childPids :: UnitTest
test_childPids =
  ([] /==) =<< childPids (Pid 1)
