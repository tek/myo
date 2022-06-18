module Myo.Test.Command.UpdateTest where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Neovim (Plugin(..))
import Path (Abs, Dir, Path)
import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Api.Variable (setVar)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.Plugin (riboPlugin, rpcHandler, sync)
import Ribosome.Test.Await (awaitEqual_)
import Ribosome.Test.Embed (integrationTestDef)
import Ribosome.Test.Orphans ()
import Ribosome.Test.Run (UnitTest)

import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.CommandSettingCodec (CommandSettingCodec(CommandSettingCodec))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (commands)
import Myo.Data.Env (Env(_tempDir))
import Myo.Env (bracketMyoTempDir)
import Myo.Plugin (handleError, variables)
import Myo.Test.Unit (MyoTest)

commands1 :: [AddSystemCommandOptions]
commands1 =
  [AddSystemCommandOptions (Ident.Str "c1") ["tail"] Nothing Nothing Nothing Nothing Nothing Nothing Nothing]

commands2 :: [AddSystemCommandOptions]
commands2 =
  [
    AddSystemCommandOptions (Ident.Str "c1") ["tails"] Nothing Nothing Nothing Nothing Nothing Nothing Nothing,
    AddSystemCommandOptions (Ident.Str "c2") ["echo"] Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    ]

codec :: [AddSystemCommandOptions] -> CommandSettingCodec
codec cmds =
  CommandSettingCodec (Just cmds) Nothing

cmdData :: Member (AtomicState Env) r => m [(Text, [Text])]
cmdData =
  fmap extract <$> atomicGets CommandState.commands
  where
    extract (Command _ ident lines' _ _ _ _ _ _) = (identText ident, lines')

$(pure [])

plugin :: Path Abs Dir -> IO (Plugin (Ribosome Env))
plugin tmp = do
  ribo <- newRibosome "myo" def { _tempDir = tmp }
  pure $ riboPlugin "myo" ribo [$(rpcHandler sync 'cmdData)] [] handleError variables

getCmdData ::
  m [(Text, [Text])]
getCmdData =
  vimCallFunction "CmdData" []

updateCommandsTest :: MyoTest ()
updateCommandsTest = do
  setVar "myo_commands" (codec commands1)
  doautocmd False "CmdlineLeave"
  awaitEqual_ [("c1", ["tail"])] getCmdData
  setVar "myo_commands" (codec commands2)
  doautocmd False "BufWinEnter"
  awaitEqual_ [("c1", ["tails"]), ("c2", ["echo"])] getCmdData

test_updateCommands :: UnitTest
test_updateCommands =
  bracketMyoTempDir run
  where
    run tmp = do
      plug <- liftIO (plugin tmp)
      integrationTestDef plug updateCommandsTest
