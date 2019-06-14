{-# OPTIONS_GHC -F -pgmF htfpp #-}

module SaveSpec (htf_thisModulesTests) where

import Chiasma.Data.Ident (Ident)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Neovim (Plugin(..))
import Path (Abs, Dir, Path)
import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Config.Setting (updateSetting)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.Plugin (riboPlugin, rpcHandler, sync)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Orphans ()
import Test.Framework

import Myo.Command.Add (myoAddSystemCommand)
import Myo.Command.Data.AddSystemCommandOptions (AddSystemCommandOptions(AddSystemCommandOptions))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandLog (CommandLog(CommandLog))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Log (appendLog, commandLog)
import Myo.Data.Env (Env(_tempDir), Myo)
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
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  m ([ByteString], ByteString)
getOutput = do
  extract <$> catchAs @CommandError Nothing (commandLog ident)
  where
    extract (Just (CommandLog prev cur)) =
      (prev, cur)
    extract _ =
      ([], "")

pushOutput ::
  MonadDeepState s CommandState m =>
  m ()
pushOutput =
  appendLog ident "log line"

$(return [])

plugin :: Path Abs Dir -> IO (Plugin (Ribosome Env))
plugin tmp = do
  ribo <- newRibosome "myo" def { _tempDir = tmp }
  return $ riboPlugin "myo" ribo (rpcHandlers ++ handlers) [] handleError variables
  where
    handlers = [$(rpcHandler sync 'getOutput), $(rpcHandler sync 'pushOutput)]

outputLog ::
  NvimE e m =>
  m ([Text], Text)
outputLog =
  vimCallFunction "GetOutput" []

saveSpec :: Myo ()
saveSpec = do
  updateSetting Settings.saveInterval 0.0
  myoAddSystemCommand $ AddSystemCommandOptions ident [] Nothing Nothing Nothing Nothing
  doautocmd "BufWritePre"
  await (gassertEqual ([], "")) outputLog
  () <- vimCallFunction "PushOutput" []
  await (gassertEqual ([], line)) outputLog
  doautocmd "BufWritePre"
  await (gassertEqual ([line], "")) outputLog
  doautocmd "BufWritePre"
  await (gassertEqual ([line], "")) outputLog
  () <- vimCallFunction "PushOutput" []
  await (gassertEqual ([line], line)) outputLog
  doautocmd "BufWritePre"
  await (gassertEqual ([line, line], "")) outputLog

test_save :: IO ()
test_save =
  bracketMyoTempDir run
  where
    run tmp = do
      plug <- plugin tmp
      integrationSpecDef plug saveSpec
