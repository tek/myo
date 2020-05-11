{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Command.CommandMenuSpec (htf_thisModulesTests) where

import Control.Concurrent.Lifted (fork, killThread)
import Control.Exception.Lifted (bracket)
import Ribosome.Api.Input (syntheticInput)
import Ribosome.Nvim.Api.IO (vimGetVar)
import Ribosome.Test.Tmux (tmuxSpecDef)
import Test.Framework

import Myo.Command.CommandMenu (myoCommands)
import Myo.Command.Data.Command (Command(Command))
import qualified Myo.Command.Data.CommandInterpreter as CommandInterpreter
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands)
import Myo.Data.Env (Myo)
import Myo.Vim.Runner (addVimRunner)

nativeChars :: [Text]
nativeChars =
  ["k", "<cr>"]

commands :: [Command]
commands =
  [entry "c1", entry "c2", entry "c3", entry "c4", entry "c4", entry "c4", entry "c4", entry "c4"]
  where
    entry n@(show -> nt) =
      Command (CommandInterpreter.Vim False Nothing) n [[qt|let g:command = '${nt}'|]] def def def False False

commandMenuSpec :: Myo ()
commandMenuSpec = do
  addVimRunner
  setL @CommandState CommandState.commands commands
  bracket (fork input) killThread (const myoCommands)
  value <- vimGetVar "command"
  gassertEqual ("c2" :: Text) value
  where
    input =
      syntheticInput (Just 0.1) nativeChars

test_commandMenu :: IO ()
test_commandMenu =
  tmuxSpecDef commandMenuSpec
