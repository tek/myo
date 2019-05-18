{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE TemplateHaskell #-}

module SaveSpec (htf_thisModulesTests) where

import Chiasma.Data.Ident (Ident)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Control.Monad.Trans.Except (ExceptT)
import Data.Default (def)
import Neovim (Neovim, Plugin(..))
import Path (Abs, Dir, Path)
import Ribosome.Api.Autocmd (doautocmd)
import Ribosome.Control.Monad.Ribo (NvimE)
import Ribosome.Control.Ribosome (newRibosome, Ribosome)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Ribosome.Plugin (riboPlugin, rpcHandler, sync)
import Ribosome.Test.Await (await)
import Ribosome.Test.Embed (integrationSpecDef)
import Ribosome.Test.Orphans ()
import Test.Framework

import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandLog (CommandLog(CommandLog))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Log (appendLog, commandLog)
import Myo.Data.Env (Env(_tempDir))
import Myo.Env (bracketMyoTempDir)
import Myo.Plugin (handleError, rpcHandlers, variables)

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
getOutput =
  extract <$> commandLog ident
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
plugin tempDir = do
  ribo <- newRibosome "myo" def { _tempDir = tempDir }
  return $ riboPlugin "myo" ribo (rpcHandlers ++ handlers) [] handleError variables
  where
    handlers = [$(rpcHandler sync 'getOutput), $(rpcHandler sync 'pushOutput)]

outputLog ::
  NvimE e m =>
  m ([Text], Text)
outputLog =
  vimCallFunction "GetOutput" []

saveSpec :: ExceptT RpcError (Neovim ()) ()
saveSpec = do
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
    run tempDir = do
      plug <- plugin tempDir
      integrationSpecDef plug saveSpec