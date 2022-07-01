module Myo.Command.Execution where

-- executionLens :: Ident -> Lens' CommandState (Maybe Execution)
-- executionLens ident =
--   #executing . Lens.at ident

-- executions ::
--   Member (AtomicState CommandState) r =>
--   Sem r (Map Ident Execution)
-- executions =
--   atomicGets CommandState.executing

-- archiveExecution ::
--   Members [AtomicState CommandState, Log] r =>
--   Ident ->
--   Sem r ()
-- archiveExecution ident =
--   traverse_ archive =<< atomicView (executionLens ident)
--   where
--     archive execution = do
--       Log.debug [exon|removing execution `#{identText ident}`|]
--       atomicModify' (#executionLog %~ (stopped execution :))
--     stopped =
--       Lens.set (#monitor . #state) Stopped

-- removeExecution ::
--   Members [AtomicState CommandState, Log] r =>
--   Ident ->
--   Sem r ()
-- removeExecution ident = do
--   archiveExecution ident
--   atomicSet (executionLens ident) Nothing

-- killExecution ::
--   Members [Executions, AtomicState CommandState, Log, Embed IO] r =>
--   Ident ->
--   Sem r ()
-- killExecution ident = do
--   Log.debug [exon|killing execution `#{identText ident}`|]
--   closeOutputSocket ident
--   removeExecution ident

-- pushExecution ::
--   Members [AtomicState CommandState, ChronosTime, Log] r =>
--   Ident ->
--   Sem r ()
-- pushExecution ident = do
--   Log.debug $ "pushing execution `" <> identText ident <> "`"
--   archiveExecution ident
--   now <- Time.now
--   atomicSet (executionLens ident) (Just (Execution ident "" [] (monitor now)))
--   where
--     monitor now =
--       ExecutionSync ExecutionState.Pending now Nothing

-- executionPid ::
--   Execution ->
--   Maybe Pid
-- executionPid Execution { state = Tracked pid } =
--   Just pid
-- executionPid _ =
--   Nothing

-- modifyExecution ::
--   Member (AtomicState CommandState) r =>
--   (Execution -> Sem r Execution) ->
--   Ident ->
--   Sem r ()
-- modifyExecution f ident =
--   atomicPut =<< Lens.mapMOf (executionLens ident . Lens._Just) f =<< atomicGet

-- modifyExecutionState ::
--   Members [AtomicState CommandState, Log] r =>
--   Ident ->
--   (ExecutionState -> Sem r ExecutionState) ->
--   Sem r ()
-- modifyExecutionState ident f =
--   modifyExecution (Lens.mapMOf #state update) ident
--   where
--     update previous = do
--       new <- f previous
--       when (previous /= new) (Log.debug (message previous new))
--       pure new
--     message previous new =
--       "changing execution state of `" <> identText ident <> "` from " <> show previous <> " to " <> show new

-- setExecutionState ::
--   Members [AtomicState CommandState, Log] r =>
--   Ident ->
--   ExecutionState ->
--   Sem r ()
-- setExecutionState ident =
--   modifyExecutionState ident . const . pure

-- storeOutputSocket ::
--   Members [AtomicState CommandState, Log] r =>
--   Socket ->
--   Ident ->
--   Sem r ()
-- storeOutputSocket socket ident = do
--   Log.debug $ "storing output socket for `" <> identText ident <> "`"
--   modifyExecution (Lens.mapMOf (#monitor . #socket) (const (pure $ Just socket))) ident

-- closeOutputSocket ::
--   Members [Executions, AtomicState CommandState, Log, Embed IO] r =>
--   Ident ->
--   Sem r ()
-- closeOutputSocket ident = do
--   Log.debug [exon|closing output socket for `#{identText ident}`|]
--   traverse_ close =<< Executions.update ident (#monitor . #socket <<.~ Nothing)
--   where
--     close socket =
--       tryAny_ (traverse_ Socket.close socket)
