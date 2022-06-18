module Myo.Command.Test where

-- import qualified Chiasma.Data.Ident as Ident (Ident(Str))
-- import Chiasma.Ui.Data.TreeModError (TreeModError)
-- import Data.Hashable (hash)
-- import Data.MessagePack (Object)
-- import Ribosome.Api.Window (currentCursor)
-- import Ribosome.Config.Setting (setting, Settings.maybe, settingOr)
-- import Ribosome.Data.PersistError (PersistError)
-- import Ribosome.Data.SettingError (SettingError)
-- import Ribosome.Nvim.Api.IO (vimCallFunction)

-- import Myo.Command.Data.Command (Command(Command))
-- import Myo.Command.Data.CommandError (CommandError)
-- import Myo.Command.Data.CommandInterpreter (CommandInterpreter(Shell, System))
-- import Myo.Command.Data.CommandState (CommandState)
-- import Myo.Command.Data.RunError (RunError)
-- import qualified Myo.Command.Data.RunError as RunError (RunError(VimTest))
-- import Myo.Command.Data.VimTestPosition (VimTestPosition(VimTestPosition))
-- import Myo.Command.Run (runCommand)
-- import Myo.Data.Env (Env)
-- import Myo.Settings (testCapture, testLang, testPane, testRunner, testShell, vimTestFileNameModifier)
-- import Myo.Ui.Data.ToggleError (ToggleError)
-- import Myo.Ui.Render (MyoRender)

-- testName :: Text
-- testName =
--   "<test>"

-- vimTestPosition ::
--   m VimTestPosition
-- vimTestPosition = do
--   fnMod <- setting vimTestFileNameModifier
--   file <- vimCallFunction "expand" [toMsgpack ("%" <> fnMod)]
--   (line, col) <- currentCursor
--   pure (VimTestPosition file line col)

-- vimTestCall ::
--   MsgpackDecode a =>
--   Text ->
--   [Object] ->
--   m a
-- vimTestCall name =
--   vimCallFunction ("test#" <> name)

-- myoTestDetermineRunner ::
--   Text ->
--   m Text
-- myoTestDetermineRunner file =
--   vimTestCall "determine_runner" [toMsgpack file]

-- myoTestExecutable ::
--   Text ->
--   m Text
-- myoTestExecutable runner =
--   vimTestCall (runner <> "#executable") []

-- myoTestBuildPosition ::
--   Text ->
--   VimTestPosition ->
--   m [Text]
-- myoTestBuildPosition runner pos =
--   vimTestCall (runner <> "#build_position") [toMsgpack ("nearest" :: Text), toMsgpack pos]

-- myoTestBuildArgs ::
--   Text ->
--   [Text] ->
--   m [Text]
-- myoTestBuildArgs runner args =
--   vimTestCall (runner <> "#build_args") [toMsgpack args]

-- vimTestCallWrap ::
--   MsgpackDecode a =>
--   Text ->
--   [Object] ->
--   m a
-- vimTestCallWrap fun =
--   vimCallFunction ("MyoTest" <> fun)

-- assembleVimTestLine ::
--   VimTestPosition ->
--   m Text
-- assembleVimTestLine position@(VimTestPosition file _ _) = do
--   runner <- vimTestCallWrap @Text "DetermineRunner" [toMsgpack file]
--   exe <- vimTestCallWrap @Text "Executable" [toMsgpack runner]
--   preArgs <- vimTestCallWrap @[Text] "BuildPosition" [toMsgpack runner, toMsgpack position]
--   args <- vimTestCallWrap "BuildArgs" [toMsgpack runner, toMsgpack preArgs]
--   pure $ unwords (exe : args)

-- vimTestLine ::
--   m Text
-- vimTestLine =
--   catchAt @RpcError (stop . RunError.VimTest . show) . assembleVimTestLine =<< vimTestPosition

-- testInterpreter :: Ident -> Maybe Ident -> CommandInterpreter
-- testInterpreter _ (Just shell) =
--   Shell shell
-- testInterpreter target _ =
--   System (Just target)

-- testIdent :: Text -> Ident
-- testIdent =
--   Ident.Str . (testName <>) . show . hash

-- updateTestCommand ::
--   Text ->
--   m Command
-- updateTestCommand testLine = do
--   runner <- setting testRunner
--   shell <- Settings.maybe testShell
--   target <- setting testPane
--   lang <- Settings.maybe testLang
--   capture <- settingOr False testCapture
--   let interpreter = testInterpreter target shell
--   pure $ Command interpreter (testIdent testLine) [testLine] (Just runner) lang (Just testName) False False capture

-- myoVimTest ::
--   MyoRender s e m =>
--   Member (AtomicState Env) r =>
--   Member (AtomicState Env) r =>
--   m ()
-- myoVimTest =
--   runCommand =<< updateTestCommand =<< vimTestLine
