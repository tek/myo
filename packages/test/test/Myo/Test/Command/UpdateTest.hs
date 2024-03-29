module Myo.Test.Command.UpdateTest where

import Polysemy.Test (UnitTest, assertEq)
import Ribosome (Execution (Sync), Handler, Rpc, resumeReport, rpcFunction, watchVariables, withHandlers)
import Ribosome.Api (nvimCallFunction, nvimSetVar)
import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Test (assertWait, testPluginEmbed)

import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions, systemOptions)
import Myo.Command.Data.Command (Command (Command), cmdLines, ident)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec (CommandSettingCodec))
import Myo.Command.Data.CommandSpec (renderCommandSpec)
import Myo.Data.CommandId (commandIdText)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
import Myo.Interpreter.Commands (interpretCommandsNoHistory)
import Myo.Plugin (variables)
import Myo.Test.Run (runMyoTestStack)

commands1 :: [AddSystemCommandOptions]
commands1 =
  [systemOptions "c1" ["tail"]]

commands2 :: [AddSystemCommandOptions]
commands2 =
  [
    systemOptions "c1" ["tails"],
    systemOptions "c2" ["echo"]
  ]

codec :: [AddSystemCommandOptions] -> CommandSettingCodec
codec cmds =
  CommandSettingCodec (Just cmds) Nothing

cmdData ::
  Member (Commands !! CommandError) r =>
  Handler r [(Text, [Text])]
cmdData =
  fmap extract <$> resumeReport Commands.all
  where
    extract (Command {ident, cmdLines}) = (commandIdText ident, renderCommandSpec cmdLines)

getCmdData ::
  Member Rpc r =>
  Sem r [(Text, [Text])]
getCmdData =
  nvimCallFunction "CmdData" []

test_updateCommands :: UnitTest
test_updateCommands =
  runMyoTestStack def $ interpretCommandsNoHistory $ withHandlers [rpcFunction "CmdData" Sync cmdData] $
  watchVariables variables $ testPluginEmbed do
    nvimSetVar "myo_commands" (codec commands1)
    doautocmd "CmdlineLeave"
    assertWait getCmdData (assertEq [("c1", ["tail"])])
    nvimSetVar "myo_commands" (codec commands2)
    doautocmd "BufWinEnter"
    assertWait getCmdData (assertEq [("c1", ["tails"]), ("c2", ["echo"])])
