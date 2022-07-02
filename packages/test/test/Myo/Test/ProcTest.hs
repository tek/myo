module Myo.Test.ProcTest where

import Conc (interpretSync)
import Path (relfile)
import qualified Polysemy.Process.SystemProcess as SystemProcess
import Polysemy.Process.SystemProcess (currentPid)
import Polysemy.Test (UnitTest, assert, assertJust)
import Process (Pid, SystemProcess, interpretSystemProcessNative_, withSystemProcess_)
import Ribosome.Test (resumeTestError, runTest)
import qualified Sync
import Time (Seconds (Seconds))

import Myo.Data.ProcError (ProcError)
import qualified Myo.Effect.Proc as Proc
import Myo.Interpreter.Proc (interpretProc)

newtype StorePid (n :: Nat) =
  StorePid { unStorePid :: Pid }
  deriving stock (Eq, Show)

test_proc :: UnitTest
test_proc =
  runTest $ interpretProc $ interpretSync @(StorePid 1) $ interpretSync @(StorePid 2) $ interpretSync @() do
    cat <- note "no cat" =<< SystemProcess.which [relfile|cat|] []
    interpretSystemProcessNative_ cat do
      pid <- currentPid
      thread1 <- async $ resumeTestError @(_ _ _) $ withSystemProcess_ $ resumeTestError @SystemProcess do
        void $ Sync.putWait (Seconds 5) . StorePid @1 =<< SystemProcess.pid
        Sync.wait @() (Seconds 5)
      thread2 <- async $ resumeTestError $ withSystemProcess_ $ resumeTestError @SystemProcess do
        void $ Sync.putWait (Seconds 5) . StorePid @2 =<< SystemProcess.pid
        Sync.wait @() (Seconds 5)
      StorePid child1 <- note "pid 1 timed out" =<< Sync.wait @(StorePid 1) (Seconds 5)
      StorePid child2 <- note "pid 2 timed out" =<< Sync.wait @(StorePid 2) (Seconds 5)
      children <- resumeHoistErrorAs @ProcError "childPids" (Proc.childPids pid)
      assertJust pid =<< resumeHoistErrorAs @ProcError "ppid" (Proc.parentPid child1)
      Sync.putWait (Seconds 5) ()
      void (await thread1)
      void (await thread2)
      assert (elem child1 children)
      assert (elem child2 children)
