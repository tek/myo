module Myo.Test.Command.CommandMenuTest where

import Chiasma.Data.Ident (identText)
import Exon (exon)
import Polysemy.Test (UnitTest, (===))
import Ribosome (interpretPersistNull)
import Ribosome.Api (nvimGetVar)
import qualified Ribosome.Menu as Menu
import Ribosome.Menu (interpretMenu)
import Ribosome.Menu.Prompt (interpretPromptInputCharList)
import Ribosome.Test (resumeTestError, testError)

import Myo.Command.CommandMenu (commandMenu, runCommand)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command)
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Interpreter.Backend.Vim (interpretBackendVim)
import Myo.Command.Interpreter.CommandLog (interpretCommandLogSetting)
import Myo.Interpreter.Controller (interpretController)
import Myo.Test.Embed (myoTest)

inputChars :: [Text]
inputChars =
  ["k", "cr"]

commands :: [Command]
commands =
  [entry "c1", entry "c2", entry "c3", entry "c4", entry "c4", entry "c4", entry "c4", entry "c4"]
  where
    entry i =
      Command.cons (CommandInterpreter.Vim False Nothing) i [[exon|let g:command = '#{identText i}'|]]

test_commandMenu :: UnitTest
test_commandMenu =
  myoTest $ interpretPersistNull $ interpretCommandLogSetting $ interpretBackendVim $ interpretController do
    atomicSet @CommandState #commands commands
    result <- resumeTestError $ interpretMenu $ interpretPromptInputCharList inputChars do
      testError (commandMenu runCommand)
    Menu.Success () === result
    value <- nvimGetVar "command"
    ("c2" :: Text) === value
