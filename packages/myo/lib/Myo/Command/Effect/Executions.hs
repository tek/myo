module Myo.Command.Effect.Executions where

import Prelude hiding (get, modify, stop)
import Process (Pid)

import qualified Myo.Command.Data.Execution as Execution
import Myo.Command.Data.Execution (Execution)
import Myo.Command.Data.ExecutionState (ExecutionState)
import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Data.CommandId (CommandId)

data Executions :: Effect where
  Get :: CommandId -> Executions m (Maybe Execution)
  Modify :: CommandId -> (ExecutionState -> ExecutionState) -> Executions m ()
  Start :: CommandId -> Bool -> Maybe UiTarget -> Executions m (Maybe CommandId)
  Stop :: CommandId -> Executions m ()
  Running :: CommandId -> Executions m Bool
  ActiveTarget :: UiTarget -> Executions m (Maybe CommandId)
  Wait :: CommandId -> Executions m ()
  Terminate :: CommandId -> Executions m ()
  WaitTerminate :: CommandId -> Executions m ()

makeSem ''Executions

setState ::
  Member Executions r =>
  CommandId ->
  ExecutionState ->
  Sem r ()
setState i s =
  modify i (const s)

pid ::
  Member Executions r =>
  CommandId ->
  Sem r (Maybe Pid)
pid i =
  (>>= Execution.pid) <$> get i

withExecution ::
  Members [Executions, Resource] r =>
  CommandId ->
  Bool ->
  Maybe UiTarget ->
  (Maybe CommandId -> Sem r a) ->
  Sem r a
withExecution cmd cmdShell target =
  bracket (start cmd cmdShell target) (const (stop cmd))
