module Myo.Command.Test where

import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Control.Monad.Catch (MonadThrow)
import Data.Hashable (hash)
import Data.MessagePack (Object)
import Ribosome.Api.Window (currentCursor)
import Ribosome.Config.Setting (setting, settingMaybe, settingOr)
import Ribosome.Data.PersistError (PersistError)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Nvim.Api.IO (vimCallFunction)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Command.Data.Command (Command(Command))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandInterpreter (CommandInterpreter(Shell, System))
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError(VimTest))
import Myo.Command.Data.VimTestPosition (VimTestPosition(VimTestPosition))
import Myo.Command.Run (runCommand)
import Myo.Data.Env (Env)
import Myo.Settings (testCapture, testLang, testPane, testRunner, testShell, vimTestFileNameModifier)
import Myo.Ui.Data.ToggleError (ToggleError)
import Myo.Ui.Render (MyoRender)

testName :: Text
testName =
  "<test>"

vimTestPosition ::
  MonadDeepError e SettingError m =>
  MonadRibo m =>
  NvimE e m =>
  m VimTestPosition
vimTestPosition = do
  fnMod <- setting vimTestFileNameModifier
  file <- vimCallFunction "expand" [toMsgpack ("%" <> fnMod)]
  (line, col) <- currentCursor
  return (VimTestPosition file line col)

vimTestCall ::
  MsgpackDecode a =>
  NvimE e m =>
  Text ->
  [Object] ->
  m a
vimTestCall name =
  vimCallFunction ("test#" <> name)

myoTestDetermineRunner ::
  NvimE e m =>
  Text ->
  m Text
myoTestDetermineRunner file =
  vimTestCall "determine_runner" [toMsgpack file]

myoTestExecutable ::
  NvimE e m =>
  Text ->
  m Text
myoTestExecutable runner =
  vimTestCall (runner <> "#executable") []

myoTestBuildPosition ::
  NvimE e m =>
  Text ->
  VimTestPosition ->
  m [Text]
myoTestBuildPosition runner pos =
  vimTestCall (runner <> "#build_position") [toMsgpack ("nearest" :: Text), toMsgpack pos]

myoTestBuildArgs ::
  NvimE e m =>
  Text ->
  [Text] ->
  m [Text]
myoTestBuildArgs runner args =
  vimTestCall (runner <> "#build_args") [toMsgpack args]

vimTestCallWrap ::
  MsgpackDecode a =>
  NvimE e m =>
  Text ->
  [Object] ->
  m a
vimTestCallWrap fun =
  vimCallFunction ("MyoTest" <> fun)

assembleVimTestLine ::
  NvimE e m =>
  VimTestPosition ->
  m Text
assembleVimTestLine position@(VimTestPosition file _ _) = do
  runner <- vimTestCallWrap @Text "DetermineRunner" [toMsgpack file]
  exe <- vimTestCallWrap @Text "Executable" [toMsgpack runner]
  preArgs <- vimTestCallWrap @[Text] "BuildPosition" [toMsgpack runner, toMsgpack position]
  args <- vimTestCallWrap "BuildArgs" [toMsgpack runner, toMsgpack preArgs]
  return $ unwords (exe : args)

vimTestLine ::
  MonadDeepError e SettingError m =>
  MonadDeepError e RunError m =>
  MonadRibo m =>
  NvimE e m =>
  m Text
vimTestLine =
  catchAt @RpcError (throwHoist . RunError.VimTest . show) . assembleVimTestLine =<< vimTestPosition

testInterpreter :: Ident -> Maybe Ident -> CommandInterpreter
testInterpreter _ (Just shell) =
  Shell shell
testInterpreter target _ =
  System (Just target)

testIdent :: Text -> Ident
testIdent =
  Ident.Str . (testName <>) . show . hash

updateTestCommand ::
  MonadDeepError e SettingError m =>
  MonadRibo m =>
  NvimE e m =>
  Text ->
  m Command
updateTestCommand testLine = do
  runner <- setting testRunner
  shell <- settingMaybe testShell
  target <- setting testPane
  lang <- settingMaybe testLang
  capture <- settingOr False testCapture
  let interpreter = testInterpreter target shell
  return $ Command interpreter (testIdent testLine) [testLine] (Just runner) lang (Just testName) False False capture

myoVimTest ::
  MonadRibo m =>
  NvimE e m =>
  RunTmux m =>
  MyoRender s e m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepError e RunError m =>
  MonadDeepError e ToggleError m =>
  MonadDeepError e TreeModError m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e SettingError m =>
  MonadDeepError e PersistError m =>
  MonadDeepState s CommandState m =>
  MonadThrow m =>
  m ()
myoVimTest =
  runCommand =<< updateTestCommand =<< vimTestLine
