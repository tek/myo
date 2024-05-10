module Myo.Test.Command.CommandMenuTest where

import Chiasma.Data.Ident (Ident (Str))
import Exon (exon)
import Polysemy.Test (UnitTest, evalMaybe, (===))
import Ribosome.Api (nvimGetVar)
import qualified Ribosome.Menu as Menu
import Ribosome.Menu (MenuResult (Success), promptInput)
import Ribosome.Menu.Prompt (PromptEvent (..))
import Ribosome.Test (resumeTestError, testError)

import Myo.Command.CommandMenu (CommandAction (Edit, Run), commandMenu)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandSpec (CommandSpec (CommandSpec))
import Myo.Command.Data.CommandTemplate (parseCommandTemplate')
import Myo.Command.Data.Param (ParamDefault (ParamDefault), ParamValues)
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunTask
import Myo.Command.Data.RunTask (RunTask (RunTask))
import Myo.Command.Edit (editCommand)
import Myo.Command.Interpreter.Backend.Generic (withBackendTrace)
import Myo.Command.Interpreter.Backend.Vim (interpretBackendVim)
import Myo.Command.Run (runIdent)
import Myo.Data.CommandId (CommandId (CommandId), commandIdText)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import Myo.Interpreter.Commands (interpretCommandsWith)
import Myo.Interpreter.Controller (interpretController, interpretControllerTransient)
import Myo.Interpreter.History (interpretHistoryTransient)
import Myo.Test.Embed (myoTest)

inputEvents :: [PromptEvent]
inputEvents =
  [Mapping "k", Mapping "<cr>"]

commands :: [Command]
commands =
  [entry "c1", entry "c2", entry "c3", entry "c4", entry "c4", entry "c4", entry "c4", entry "c4"]
  where
    entry i =
      Command.cons (CommandInterpreter.Vim False Nothing) i [[exon|let g:command = '#{commandIdText i}'|]]

test_commandMenu :: UnitTest
test_commandMenu =
  myoTest $ interpretBackendVim $ interpretControllerTransient [] do
    resumeTestError @Commands (traverse_ Commands.add commands)
    Menu.Success (Run ident) <- promptInput inputEvents do
      testError @CommandError commandMenu
    testError (runIdent ident mempty)
    value <- nvimGetVar "command"
    ("c2" :: Text) === value

params :: ParamValues
params = [("par1", "value 1"), ("par2", "value 2"), ("par3", "value 3")]

exeId :: CommandId
exeId = CommandId (Str "exe1")

commandEdit :: Command
commandEdit =
  Command.consSpec (CommandInterpreter.System Nothing) "1" spec
  where
    spec = CommandSpec tpl (coerce params)
    tpl = parseCommandTemplate' ["garbage"]

editEvents :: [PromptEvent]
editEvents =
  [
    Mapping "k",
    Mapping "e",
    Update "nest: {par3}",
    Mapping "<esc>",
    Mapping "k",
    Mapping "k",
    Mapping "e",
    Update "{par1} {par2}",
    Mapping "<cr>"
  ]

test_commandMenuEdit :: UnitTest
test_commandMenuEdit =
  myoTest do
    withBackendTrace $ interpretHistoryTransient [] $
      interpretCommandsWith [commandEdit] $ interpretController $ testError @CommandError $ testError @RunError do
        RunTask {compiled = value1} <- evalMaybe =<< edit
        ["value 1 nest: value 3"] === value1
  where
    edit = do
      Success (Edit i) <- promptInput [Mapping "e"] do
        commandMenu
      promptInput editEvents do
        editCommand i
      atomicGets head
