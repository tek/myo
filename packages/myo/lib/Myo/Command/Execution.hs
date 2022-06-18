module Myo.Command.Execution where

-- import Chiasma.Data.Ident (Ident, identText)
-- import Conc (lock)
-- import Control.Lens (Lens', (%~))
-- import qualified Control.Lens as Lens (_Just, at, mapMOf, set, view)
-- import Exon (exon)
-- import qualified Log
-- import Network.Socket (Socket)
-- import qualified Network.Socket as Socket (close)
-- import qualified Time

-- import Myo.AtomicState (atomicSet, atomicView)
-- import qualified Myo.Command.Data.CommandState as CommandState
-- import Myo.Command.Data.CommandState (CommandState)
-- import Myo.Command.Data.Execution (Execution (Execution), ExecutionMonitor (ExecutionMonitor), ExecutionState (..))
-- import qualified Myo.Command.Data.Execution as ExecutionState (ExecutionState (..))
-- import Myo.Command.Data.ExecutionLock (ExecutionLock (ExecutionLock))
-- import Myo.Command.Data.Pid (Pid)

-- executionLens :: Ident -> Lens' CommandState (Maybe Execution)
-- executionLens ident =
--   #executing . Lens.at ident

-- executions ::
--   Member (AtomicState CommandState) r =>
--   Sem r (Map Ident Execution)
-- executions =
--   atomicGets CommandState.executing

-- archiveExecution ::
--   Member (AtomicState CommandState) r =>
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
--   Member (AtomicState CommandState) r =>
--   Ident ->
--   Sem r ()
-- removeExecution ident = do
--   archiveExecution ident
--   atomicSet (executionLens ident) Nothing

-- killExecution ::
--   Member (AtomicState CommandState) r =>
--   Ident ->
--   Sem r ()
-- killExecution ident = do
--   Log.debug [exon|killing execution `#{identText ident}`|]
--   closeOutputSocket ident
--   removeExecution ident

-- pushExecution ::
--   Member (AtomicState CommandState) r =>
--   Ident ->
--   IO ExecutionState ->
--   Sem r ()
-- pushExecution ident checkPending = do
--   Log.debug $ "pushing execution `" <> identText ident <> "`"
--   archiveExecution ident
--   now <- Time.now
--   atomicSet (executionLens ident) (Just (Execution ident "" [] (monitor now)))
--   where
--     monitor now =
--       ExecutionMonitor ExecutionState.Pending now Nothing checkPending

-- findExecution ::
--   Member (AtomicState CommandState) r =>
--   Ident ->
--   Sem r (Maybe Execution)
-- findExecution ident =
--   gets $ Lens.view $ executionLens ident

-- executionRunning ::
--   Execution ->
--   Sem r Bool
-- executionRunning (Execution _ _ _ (ExecutionMonitor state _ _ _)) =
--   check state
--   where
--     check (Tracked _) =
--       undefined
--       -- processExists pid
--     check (Starting _) =
--       pure False
--     check Running =
--       pure True
--     check Pending =
--       pure False
--     check Unknown =
--       pure False
--     check Stopped =
--       pure False

-- executionActive ::
--   Execution ->
--   Sem r Bool
-- executionActive (Execution _ _ _ (ExecutionMonitor state _ _ _)) =
--   check state
--   where
--     check (Tracked _) =
--       undefined
--       -- processExists pid
--     check (Starting _) =
--       undefined
--       -- processExists pid
--     check Running =
--       pure True
--     check Pending =
--       pure True
--     check Unknown =
--       pure False
--     check Stopped =
--       pure False

-- isCommandRunning ::
--   Member (AtomicState CommandState) r =>
--   Ident ->
--   Sem r Bool
-- isCommandRunning =
--   maybe (pure False) executionRunning <=< findExecution

-- isCommandActive ::
--   Member (AtomicState CommandState) r =>
--   Ident ->
--   Sem r Bool
-- isCommandActive =
--   maybe (pure False) executionActive <=< findExecution

-- executionPid ::
--   Execution ->
--   Maybe Pid
-- executionPid (Execution _ _ _ (ExecutionMonitor (Tracked pid) _ _ _)) =
--   Just pid
-- executionPid _ =
--   Nothing

-- -- TODO this lock is pointless, it doesn't protect the state, only this function
-- modifyExecution ::
--   Member (AtomicState CommandState) r =>
--   (Execution -> Sem r Execution) ->
--   Ident ->
--   Sem r ()
-- modifyExecution f ident =
--   lock ExecutionLock do
--     atomicPut =<< Lens.mapMOf (executionLens ident . Lens._Just) f =<< atomicGet

-- modifyExecutionState ::
--   Member (AtomicState CommandState) r =>
--   Ident ->
--   (ExecutionState -> Sem r ExecutionState) ->
--   Sem r ()
-- modifyExecutionState ident f =
--   modifyExecution (Lens.mapMOf (#monitor . #state) update) ident
--   where
--     update previous = do
--       new <- f previous
--       when (previous /= new) (Log.debug (message previous new))
--       pure new
--     message previous new =
--       "changing execution state of `" <> identText ident <> "` from " <> show previous <> " to " <> show new

-- setExecutionState ::
--   Member (AtomicState CommandState) r =>
--   Ident ->
--   ExecutionState ->
--   Sem r ()
-- setExecutionState ident =
--   modifyExecutionState ident . const . pure

-- storeOutputSocket ::
--   Member (AtomicState CommandState) r =>
--   Socket ->
--   Ident ->
--   Sem r ()
-- storeOutputSocket socket ident = do
--   Log.debug $ "storing output socket for `" <> identText ident <> "`"
--   modifyExecution (Lens.mapMOf (#monitor . #socket) (const (pure $ Just socket))) ident

-- closeOutputSocket ::
--   Member (AtomicState CommandState) r =>
--   Ident ->
--   Sem r ()
-- closeOutputSocket ident = do
--   Log.debug $ "closing output socket for `" <> identText ident <> "`"
--   lock ExecutionLock do
--     atomicPut =<< Lens.mapMOf (executionLens ident . Lens._Just . #monitor . #socket) close =<< atomicGet
--   where
--     close socket =
--       Nothing <$ tryAny_ (traverse_ Socket.close socket)
