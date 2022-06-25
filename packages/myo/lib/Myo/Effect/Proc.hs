module Myo.Effect.Proc where

import Myo.Command.Data.Pid (Pid)

data Proc :: Effect where
  ChildPids :: Pid -> Proc m [Pid]
  ParentPids :: Pid -> Proc m (NonEmpty Pid)
  Exists :: Pid -> Proc m Bool

makeSem ''Proc

childPid ::
  Member Proc r =>
  Pid ->
  Sem r (Maybe Pid)
childPid p =
  head <$> childPids p
