module Myo.Command.Parse where

import Chiasma.Data.Ident (Ident)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Ribosome (Handler, Rpc, RpcError, Scratch, SettingError, Settings, mapHandlerError)
import Ribosome.Errors (pluginHandlerErrors)
import qualified Ribosome.Settings as Settings

import Myo.Command.Command (commandByIdent, latestCommand)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command, ident, lang), CommandLanguage (CommandLanguage))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import Myo.Command.Data.LogDir (LogDir)
import Myo.Command.Data.OutputState (OutputState (OutputState))
import Myo.Command.Data.ParseOptions (ParseOptions (ParseOptions))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.Log (commandLogByName)
import Myo.Command.Output (compileAndRenderReport)
import qualified Myo.Effect.Controller as Controller
import Myo.Effect.Controller (Controller)
import qualified Myo.Output.Data.EventIndex as EventIndex
import qualified Myo.Output.Data.OutputError as OutputError
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.ParsedOutput as ParsedOutput
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import qualified Myo.Output.Effect.Parsing as Parsing
import Myo.Output.Effect.Parsing (Parsing)
import qualified Myo.Settings as Settings
import qualified Log
import Exon (exon)

selectCommand ::
  Members [AtomicState CommandState, Stop CommandError] r =>
  Maybe Ident ->
  Sem r Command
selectCommand =
  maybe latestCommand (commandByIdent "selectCommand")

-- removeTerminalCodesRE :: SearchReplace RE Text
-- removeTerminalCodesRE =
--   [ed|\e\[[0-9;?]*[a-zA-z]///|]

-- removeLineFeedRE :: SearchReplace RE Text
-- removeLineFeedRE =
--   [ed|\r///|]

-- sanitizeOutput :: Text -> Text
-- sanitizeOutput =
--   (*=~/ removeTerminalCodesRE) . (*=~/ removeLineFeedRE)

-- commandOutputResult ::
--   Text ->
--   Maybe ByteString ->
--   m Text
-- commandOutputResult ident =
--   maybe err convert
--   where
--     convert =
--       pure . sanitizeOutput . decodeUtf8
--     err =
--       stop $ OutputError.NoOutput ident

-- commandOutput ::
--   Ident ->
--   Maybe Word ->
--   Bool ->
--   Sem r Text
-- commandOutput ident index capture = do
--   name <- displayNameByIdent ident
--   commandOutputResult name . (>>= select) =<< CommandLog.get (cmd ^. #ident)
--   where
--     select =
--       maybe (Just . view CommandLog.current) selectPrevious (fromIntegral <$> index)
--     selectPrevious i =
--       firstOf (CommandLog.previous . element i)

commandOutputByName ::
  Members [CommandLog, AtomicState CommandState, Stop OutputError] r =>
  Text ->
  Text ->
  Sem r Text
commandOutputByName context name =
  stopNote (OutputError.NoOutput name) =<< mapStop OutputError.Command (commandLogByName context name)

projectLanguage ::
  Member (Settings !! SettingError) r =>
  Sem r (Maybe CommandLanguage)
projectLanguage =
  fmap CommandLanguage <$> Settings.maybe Settings.proteomeMainType

commandParseLang ::
  Members [Settings !! SettingError, Stop OutputError] r =>
  Command ->
  Sem r CommandLanguage
commandParseLang = \case
  Command {lang = Just l} ->
    pure l
  Command {ident, lang = Nothing} ->
    stopNote (OutputError.NoLang ident) =<< projectLanguage

parseCommand ::
  Members [Settings !! SettingError, Parsing !! OutputError] r =>
  Members [Controller !! RunError, CommandLog, Reader LogDir, AtomicState CommandState, Stop OutputError, Log] r =>
  Command ->
  Sem r (Maybe (NonEmpty ParsedOutput))
parseCommand cmd =
  mapStop OutputError.Command do
    Log.debug [exon|Parsing command #{show cmd}|]
    lang <- commandParseLang cmd
    -- cmd <- mainCommandOrHistory ident
    when (cmd ^. #capture) $ mapStop OutputError.Run do
      restop (Controller.captureOutput (cmd ^. #ident))
    restop @OutputError $ runMaybeT do
      out <- MaybeT (CommandLog.get ident)
      MaybeT (Parsing.parse lang out) <|> (MaybeT . Parsing.parse lang =<< MaybeT (CommandLog.getPrev ident))
  where
    ident =
      cmd ^. #ident

storeParseResult ::
  Member (AtomicState CommandState) r =>
  Ident ->
  NonEmpty ParsedOutput ->
  Sem r ()
storeParseResult ident parsed =
  atomicSet #output (Just outputState)
  where
    outputState =
      OutputState ident (toList syntax) events (EventIndex.Absolute 0) Nothing
    syntax =
      ParsedOutput.syntax <$> parsed
    events =
      foldMap ParsedOutput.events parsed

myoParse ::
  Members [Rpc !! RpcError, Controller !! RunError, Reader LogDir, CommandLog, Parsing !! OutputError, Embed IO] r =>
  Members [Settings !! SettingError, Scratch !! RpcError, AtomicState CommandState, Log] r =>
  ParseOptions ->
  Handler r ()
myoParse (ParseOptions _ ident _) =
  pluginHandlerErrors $ mapHandlerError @OutputError $ mapHandlerError @CommandError do
    cmd <- selectCommand ident
    parsedOutput <- stopNote (OutputError.NoEvents (Command.name cmd)) =<< parseCommand cmd
    storeParseResult (cmd ^. #ident) parsedOutput
    display <- Settings.get Settings.displayResult
    when display compileAndRenderReport

myoParseLatest ::
  Members [Rpc !! RpcError, Controller !! RunError, Reader LogDir, CommandLog, Parsing !! OutputError, Embed IO] r =>
  Members [Settings !! SettingError, Scratch !! RpcError, AtomicState CommandState, Log] r =>
  Handler r ()
myoParseLatest =
  myoParse def
