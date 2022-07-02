module Myo.Test.Command.CommandMenuTest where

import Ribosome.Api.Input (syntheticInput)

import Myo.Command.Data.Command (Command(Command))
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands)

nativeChars :: [Text]
nativeChars =
  ["k", "<cr>"]

commands :: [Command]
commands =
  [entry "c1", entry "c2", entry "c3", entry "c4", entry "c4", entry "c4", entry "c4", entry "c4"]
  where
    entry nt =
      Command (CommandInterpreter.Vim False Nothing) nt [[exon|let g:command = '#{nt}'|]] def def def False False False

commandMenuTest :: Sem r ()
commandMenuTest = do
  lift addVimRunner
  setL @CommandState CommandState.commands commands
  bracket (fork input) killThread (const (lift myoCommands))
  value <- vimGetVar "command"
  ("c2" :: Text) === value
  where
    input =
      syntheticInput (Just 0.1) nativeChars

test_commandMenu :: UnitTest
test_commandMenu =
  tmuxTestDef commandMenuTest
