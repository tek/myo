module Myo.Test.Command.CommandMenuTest where

import Exon (exon)
import Polysemy.Test (UnitTest, (===))
import Ribosome.Api (nvimGetVar)
import qualified Ribosome.Menu as Menu
import Ribosome.Menu (promptInput)
import Ribosome.Menu.Prompt (PromptEvent (Mapping))
import Ribosome.Test (resumeTestError, testError)

import Myo.Command.CommandMenu (commandMenu)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Interpreter.Backend.Vim (interpretBackendVim)
import Myo.Command.Run (runIdent)
import Myo.Data.CommandId (commandIdText)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import Myo.Interpreter.Controller (interpretControllerTransient)
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
    Menu.Success ident <- promptInput inputEvents do
      testError @CommandError commandMenu
    testError (runIdent ident mempty)
    value <- nvimGetVar "command"
    ("c2" :: Text) === value
