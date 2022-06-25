module Myo.Test.SaveTest where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Neovim (Plugin(..))
import Path (Abs, Dir, Path)
import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Config.Setting (Settings.update)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.Plugin (riboPlugin, rpcHandler, sync)
import Ribosome.Test.Await (awaitEqual_)
import Ribosome.Test.Embed (integrationTestDef)
import Ribosome.Test.Orphans ()

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandLog (CommandLog(CommandLog))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Log (appendLog, commandLog)
import Myo.Data.Env (Env(_tempDir))
import Myo.Env (bracketMyoTempDir)
import Myo.Plugin (handleError, rpcHandlers, variables)
import qualified Myo.Settings as Settings (saveInterval)

ident :: Ident
ident =
  Ident.Str "cmd"

line :: Text
line =
  "log line"

getOutput ::
  Member (AtomicState Env) r =>
  m ([ByteString], ByteString)
getOutput = do
  extract <$> catchAs @CommandError Nothing (commandLog ident)
  where
    extract (Just (CommandLog prev cur)) =
      (prev, cur)
    extract _ =
      ([], "")

pushOutput ::
  Member (AtomicState Env) r =>
  m ()
pushOutput =
  appendLog ident "log line"

$(pure [])

plugin :: Path Abs Dir -> IO (Plugin (Ribosome Env))
plugin tmp = do
  ribo <- newRibosome "myo" def { _tempDir = tmp }
  pure $ riboPlugin "myo" ribo (rpcHandlers ++ handlers) [] handleError variables
  where
    handlers = [$(rpcHandler sync 'getOutput), $(rpcHandler sync 'pushOutput)]

outputLog ::
  m ([Text], Text)
outputLog =
  vimCallFunction "GetOutput" []

saveTest :: Sem r ()
saveTest = do
  Settings.update Settings.saveInterval 0.0
  myoAddSystemCommand $ AddSystemCommandOptions ident [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing
  doautocmd False "BufWritePre"
  awaitEqual_ ([], "") outputLog
  () <- vimCallFunction "PushOutput" []
  awaitEqual_ ([], line) outputLog
  doautocmd False "BufWritePre"
  awaitEqual_ ([line], "") outputLog
  doautocmd False "BufWritePre"
  awaitEqual_ ([line], "") outputLog
  () <- vimCallFunction "PushOutput" []
  awaitEqual_ ([line], line) outputLog
  doautocmd False "BufWritePre"
  awaitEqual_ ([line, line], "") outputLog

test_save :: UnitTest
test_save =
  bracketMyoTempDir run
  where
    run tmp = do
      plug <- liftIO (plugin tmp)
      integrationTestDef plug saveTest
