module Myo.Command.Effect.Executions where

import Chiasma.Data.Ident (Ident)
import Data.Generics.Labels ()
import Prelude hiding (get, modify)
import Process (Pid)

import qualified Myo.Command.Data.Execution as Execution
import Myo.Command.Data.Execution (Execution)
import Myo.Command.Data.ExecutionState (ExecutionState)

data Executions :: Effect where
  Get :: Ident -> Executions m (Maybe Execution)
  Modify :: Ident -> (Execution -> (a, Maybe Execution)) -> Executions m (Maybe a)
  Start :: Ident -> Executions m ()
  Stop :: Ident -> Executions m ()
  Running :: Ident -> Executions m Bool
  Active :: Ident -> Executions m Bool
  Wait :: Ident -> Executions m ()
  Kill :: Ident -> Executions m ()
  WaitKill :: Ident -> Executions m ()

makeSem ''Executions

modify_ ::
  Member Executions r =>
  Ident ->
  (Execution -> Maybe Execution) ->
  Sem r ()
modify_ i f =
  void (modify i (((),) . f))

modifyState ::
  Member Executions r =>
  Ident ->
  (ExecutionState -> ExecutionState) ->
  Sem r ()
modifyState i f =
  modify_ i (Just . over #state f)

setState ::
  Member Executions r =>
  Ident ->
  ExecutionState ->
  Sem r ()
setState i s =
  modifyState i (const s)

update ::
  Member Executions r =>
  Ident ->
  (Execution -> (a, Execution)) ->
  Sem r (Maybe a)
update i f =
  modify i (second Just . f)

pid ::
  Member Executions r =>
  Ident ->
  Sem r (Maybe Pid)
pid i =
  (>>= Execution.pid) <$> get i
