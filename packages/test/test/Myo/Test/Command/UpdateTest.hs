module Myo.Test.Command.UpdateTest where

import Polysemy.Test (UnitTest, assertEq)
import Ribosome (Execution (Sync), Handler, Rpc, rpcFunction, watchVariables, withHandlers)
import Ribosome.Api (nvimCallFunction, nvimSetVar)
import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Test (assertWait, testPluginEmbed)

import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions (AddSystemCommandOptions))
import Myo.Command.Data.Command (Command (Command), cmdLines, ident)
import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec (CommandSettingCodec))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Data.CommandId (commandIdText)
import Myo.Plugin (variables)
import Myo.Test.Run (runMyoTestStack)

commands1 :: [AddSystemCommandOptions]
commands1 =
  [AddSystemCommandOptions "c1" ["tail"] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing]

commands2 :: [AddSystemCommandOptions]
commands2 =
  [
    AddSystemCommandOptions "c1" ["tails"] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing,
    AddSystemCommandOptions "c2" ["echo"] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    ]

codec :: [AddSystemCommandOptions] -> CommandSettingCodec
codec cmds =
  CommandSettingCodec (Just cmds) Nothing

cmdData ::
  Member (AtomicState CommandState) r =>
  Handler r [(Text, [Text])]
cmdData =
  fmap extract <$> atomicView #commands
  where
    extract (Command {ident, cmdLines}) = (commandIdText ident, cmdLines)

getCmdData ::
  Member Rpc r =>
  Sem r [(Text, [Text])]
getCmdData =
  nvimCallFunction "CmdData" []

test_updateCommands :: UnitTest
test_updateCommands =
  runMyoTestStack def $ withHandlers [rpcFunction "CmdData" Sync cmdData] $ watchVariables variables $ testPluginEmbed do
    nvimSetVar "myo_commands" (codec commands1)
    doautocmd "CmdlineLeave"
    assertWait getCmdData (assertEq [("c1", ["tail"])])
    nvimSetVar "myo_commands" (codec commands2)
    doautocmd "BufWinEnter"
    assertWait getCmdData (assertEq [("c1", ["tails"]), ("c2", ["echo"])])
