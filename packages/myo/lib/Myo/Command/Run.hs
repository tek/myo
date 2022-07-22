module Myo.Command.Run where

import qualified Chiasma.Data.Ident as Ident
import Chiasma.Data.Ident (Ident, generateIdent)
import qualified Data.Text as Text
import Ribosome (Handler, mapHandlerError, resumeHandlerError, Args (Args))

import Myo.Command.Command (commandByIdentOrName, mayCommandByIdent, shellCommand, systemCommand)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (..))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.RunError as RunError
import Myo.Command.Data.RunError (RunError)
import Myo.Command.Data.RunLineOptions (RunLineOptions (RunLineOptions), line)
import Myo.Command.History (lookupHistory)
import Myo.Data.Maybe (orFalse)
import qualified Myo.Effect.Controller as Controller
import Myo.Effect.Controller (Controller)

myoRunIdent ::
  Member (Controller !! RunError) r =>
  Ident ->
  Handler r ()
myoRunIdent i =
  resumeHandlerError do
    Controller.runIdent i

myoRun ::
  Members [Controller !! RunError, AtomicState CommandState] r =>
  Text ->
  Handler r ()
myoRun ident =
  resumeHandlerError @Controller $ mapHandlerError do
    Controller.runCommand =<< commandByIdentOrName "run" (Text.strip ident)

reRun ::
  Members [Controller, AtomicState CommandState, Stop CommandError] r =>
  Either Ident Int ->
  Sem r ()
reRun =
  Controller.runCommand <=< lookupHistory

myoReRun ::
  Members [Controller !! RunError, AtomicState CommandState] r =>
  Either Ident Int ->
  Handler r ()
myoReRun spec =
  resumeHandlerError @Controller $ mapHandlerError do
    reRun spec

defaultTarget :: Ident
defaultTarget =
  Ident.Str "make"

myoLine ::
  Members [Controller !! RunError, AtomicState CommandState, Embed IO] r =>
  RunLineOptions ->
  Handler r ()
myoLine (RunLineOptions mayLine mayLines mayTarget runner lang skipHistory kill capture) =
  resumeHandlerError @Controller $ mapHandlerError do
    ident <- generateIdent
    lines' <- stopNote RunError.NoLinesSpecified (mayLines <|> (pure <$> mayLine))
    target <- maybe (pure (Right defaultTarget)) findTarget mayTarget
    Controller.runCommand (cmd ident target lines')
  where
    cmd ident target cmdLines =
      (cons target ident cmdLines) { runner, lang, skipHistory = orFalse skipHistory, kill = orFalse kill, capture = orFalse capture }
    cons =
      either shellCommand (systemCommand . Just)
    findTarget target =
      maybe (Right target) (Left . Command.ident) <$> mayCommandByIdent target

myoLineCmd ::
  Members [Controller !! RunError, AtomicState CommandState, Embed IO] r =>
  Args ->
  Handler r ()
myoLineCmd (Args cmdLine) =
  myoLine def { line = Just cmdLine }
