module Myo.Interpreter.Controller where

import Chiasma.Data.Ident (identText)
import Exon (exon)
import Generics.SOP (SList (SCons, SNil), SListI, sList)
import Log (Severity (Error))
import Ribosome (ErrorMessage (ErrorMessage), HostError, reportError)

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.LogDir (LogDir)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunTask (RunTask)
import Myo.Command.RunTask (runTask)
import Myo.Effect.Controller (Controller (RunCommand))
import qualified Myo.Effect.Executor as Executor
import Myo.Effect.Executor (Executor)

class MapExecutors (execs :: [Type]) (r :: EffectRow) where
  mapExecutors :: RunTask -> SList execs -> Sem r (Maybe [Text])

instance MapExecutors '[] r where
  mapExecutors _ SNil =
    pure Nothing

instance (
  MapExecutors execs r
  ) => MapExecutors (a : execs) (Executor a : r) where
    mapExecutors task SCons =
      Executor.accept task >>= \case
        Just a -> Executor.run a
        Nothing -> raise (mapExecutors task (sList @execs))

handleError ::
  Member (DataLog HostError) r =>
  Command ->
  [Text] ->
  Sem r ()
handleError Command {ident} errors =
  reportError (Just "command") (message errors Error)
  where
    message = \case
      e ->
        ErrorMessage [exon|#{identText ident} failed#{foldMap userMessage (head e)}|]
        (["Controller: command failed", identText ident] <> e)
    userMessage m =
      [exon|: #{m}|]

interpretController ::
  ∀ (execs :: [Type]) r .
  SListI execs =>
  MapExecutors execs r =>
  Members [Reader LogDir, AtomicState CommandState, DataLog HostError] r =>
  InterpreterFor (Controller !! RunError) r
interpretController =
  interpretResumable \case
    RunCommand cmd -> do
      task <- mapStop RunError.Command (runTask cmd)
      traverse_ (handleError cmd) =<< raise (mapExecutors task (sList @execs))
