module Myo.Command.Test where

import Chiasma.Data.Ident (Ident (Str))
import Data.Hashable (hash)
import Data.MessagePack (Object)
import Exon (exon)
import Ribosome (Handler, MsgpackDecode, Rpc, RpcError, Settings, mapReport, resumeReport, toMsgpack)
import Ribosome.Api (nvimCallFunction)
import Ribosome.Api.Window (currentCursor)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Effect.Settings as Settings

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command, capture, displayName, lang, runner)
import Myo.Command.Data.CommandInterpreter (CommandInterpreter (Shell, System))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Data.RunError as RunError (RunError (VimTest))
import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Command.Data.VimTestPosition (VimTestPosition (VimTestPosition))
import Myo.Data.CommandId (CommandId (CommandId))
import qualified Myo.Effect.Controller as Controller
import Myo.Effect.Controller (Controller)
import Myo.Settings (testCapture, testLang, testPane, testRunner, testShell, vimTestFileNameModifier)

testName :: Text
testName =
  "<test>"

vimTestPosition ::
  Members [Settings, Rpc] r =>
  Sem r VimTestPosition
vimTestPosition = do
  fnMod <- Settings.get vimTestFileNameModifier
  file <- nvimCallFunction "expand" [toMsgpack ("%" <> fnMod)]
  (line, col) <- currentCursor
  pure (VimTestPosition file line col)

vimTestCall ::
  Member Rpc r =>
  MsgpackDecode a =>
  Text ->
  [Object] ->
  Sem r a
vimTestCall name =
  nvimCallFunction ("test#" <> name)

myoTestDetermineRunner ::
  Member (Rpc !! RpcError) r =>
  Text ->
  Handler r Text
myoTestDetermineRunner file =
  resumeReport do
    vimTestCall "determine_runner" [toMsgpack file]

myoTestExecutable ::
  Member (Rpc !! RpcError) r =>
  Text ->
  Handler r Text
myoTestExecutable runner =
  resumeReport do
    vimTestCall (runner <> "#executable") []

myoTestBuildPosition ::
  Member (Rpc !! RpcError) r =>
  Text ->
  VimTestPosition ->
  Handler r [Text]
myoTestBuildPosition runner pos =
  resumeReport do
    vimTestCall (runner <> "#build_position") [toMsgpack ("nearest" :: Text), toMsgpack pos]

myoTestBuildArgs ::
  Member (Rpc !! RpcError) r =>
  Text ->
  [Text] ->
  Handler r [Text]
myoTestBuildArgs runner args =
  resumeReport do
    vimTestCall (runner <> "#build_args") [toMsgpack args]

vimTestCallWrap ::
  ∀ a r .
  Member Rpc r =>
  MsgpackDecode a =>
  Text ->
  [Object] ->
  Sem r a
vimTestCallWrap fun =
  nvimCallFunction ("MyoTest" <> fun)

assembleVimTestLine ::
  Member Rpc r =>
  VimTestPosition ->
  Sem r Text
assembleVimTestLine position@(VimTestPosition file _ _) = do
  runner <- vimTestCallWrap @Text "DetermineRunner" [toMsgpack file]
  exe <- vimTestCallWrap @Text "Executable" [toMsgpack runner]
  preArgs <- vimTestCallWrap @[Text] "BuildPosition" [toMsgpack runner, toMsgpack position]
  args <- vimTestCallWrap "BuildArgs" [toMsgpack runner, toMsgpack preArgs]
  pure $ unwords (exe : args)

vimTestLine ::
  Members [Settings, Rpc !! RpcError, Stop RunError] r =>
  Sem r Text
vimTestLine =
  resumeHoist (RunError.VimTest . show) do
    assembleVimTestLine =<< vimTestPosition

testInterpreter :: UiTarget -> Maybe CommandId -> CommandInterpreter
testInterpreter _ (Just shell) =
  Shell shell
testInterpreter target _ =
  System (Just target)

testIdent :: Text -> CommandId
testIdent name =
  CommandId (Str [exon|#{testName}-#{show (abs (hash name))}|])

updateTestCommand ::
  Members [Settings, Settings !! SettingError] r =>
  Text ->
  Sem r Command
updateTestCommand testLine = do
  runner <- Settings.get testRunner
  shell <- Settings.maybe testShell
  target <- Settings.get testPane
  lang <- Settings.maybe testLang
  capture <- Settings.or False testCapture
  let interpreter = testInterpreter target shell
  pure (Command.cons interpreter (testIdent testLine) [testLine]) {
    runner = Just runner,
    displayName = Just testName,
    capture,
    lang
    }

myoVimTest ::
  Members [Controller !! RunError, Settings !! SettingError, Rpc !! RpcError] r =>
  Handler r ()
myoVimTest =
  resumeReport @Settings $ resumeReport @Controller do
    Controller.runCommand =<< updateTestCommand =<< mapReport vimTestLine
