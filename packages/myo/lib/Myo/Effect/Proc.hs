module Myo.Effect.Proc where

import Process (Pid)

data Proc :: Effect where
  ChildPids :: Pid -> Proc m [Pid]
  ParentPids :: Pid -> Proc m [Pid]
  Exists :: Pid -> Proc m Bool

makeSem ''Proc

childPid ::
  Member Proc r =>
  Pid ->
  Sem r (Maybe Pid)
childPid p =
  head <$> childPids p

parentPid ::
  Member Proc r =>
  Pid ->
  Sem r (Maybe Pid)
parentPid p =
  head <$> parentPids p
