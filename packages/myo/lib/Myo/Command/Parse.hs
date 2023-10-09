module Myo.Command.Parse where

import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Exon (exon)
import qualified Log
import Ribosome (Handler, Rpc, RpcError, Scratch, SettingError, Settings, mapReport, pluginLogReports)
import qualified Ribosome.Settings as Settings

import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command (Command, ident, lang), CommandLanguage (CommandLanguage))
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.LogDir (LogDir)
import Myo.Command.Data.OutputState (OutputState (OutputState))
import Myo.Command.Data.ParseOptions (ParseOptions (ParseOptions))
import Myo.Command.Data.RunError (RunError)
import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.Log (commandLogByName)
import Myo.Command.Output (compileAndRenderReport)
import Myo.Data.CommandId (CommandId)
import Myo.Data.CommandName (CommandName (CommandName))
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)
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

selectCommand ::
  Member Commands r =>
  Maybe CommandId ->
  Sem r Command
selectCommand ident =
  maybe Commands.latest Commands.queryId ident

commandOutputByName ::
  Members [Commands !! CommandError, CommandLog, Stop OutputError] r =>
  CommandName ->
  Sem r Text
commandOutputByName name =
  stopNote err =<< resumeHoistAs err (commandLogByName name)
  where
    err = OutputError.NoOutput (coerce name)

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
  Members [Controller !! RunError, CommandLog, Reader LogDir, Stop OutputError, Log] r =>
  Command ->
  Sem r (Maybe (NonEmpty ParsedOutput))
parseCommand cmd =
  mapStop OutputError.Command do
    Log.debug [exon|Parsing command #{show cmd}|]
    lang <- commandParseLang cmd
    when (cmd ^. #capture) $ mapStop OutputError.Run do
      restop (Controller.captureOutput (cmd ^. #ident))
    restop @OutputError $ runMaybeT do
      out <- MaybeT (CommandLog.get ident)
      MaybeT (Parsing.parse lang out) <|> (MaybeT . Parsing.parse lang =<< MaybeT (CommandLog.getPrev ident))
  where
    ident =
      cmd ^. #ident

storeParseResult ::
  Member Commands r =>
  CommandId ->
  NonEmpty ParsedOutput ->
  Sem r ()
storeParseResult ident parsed =
  Commands.setCurrentOutput outputState
  where
    outputState =
      OutputState ident (toList syntax) events (EventIndex.Absolute 0) Nothing
    syntax =
      (.syntax) <$> parsed
    events =
      foldMap (.events) parsed

myoParse ::
  Members [Rpc !! RpcError, Controller !! RunError, Reader LogDir, CommandLog, Parsing !! OutputError, Embed IO] r =>
  Members [Commands !! CommandError, Settings !! SettingError, Scratch !! RpcError, Log] r =>
  ParseOptions ->
  Handler r ()
myoParse (ParseOptions _ ident _) =
  pluginLogReports $ mapReport @OutputError $ mapReport @CommandError do
    cmd <- restop (selectCommand ident)
    parsedOutput <- stopNote (OutputError.NoEvents (Command.describe cmd)) =<< parseCommand cmd
    restop (storeParseResult (cmd ^. #ident) parsedOutput)
    display <- Settings.get Settings.displayResult
    when display (restop compileAndRenderReport)

myoParseLatest ::
  Members [Rpc !! RpcError, Controller !! RunError, Reader LogDir, CommandLog, Parsing !! OutputError, Embed IO] r =>
  Members [Commands !! CommandError, Settings !! SettingError, Scratch !! RpcError, Log] r =>
  Handler r ()
myoParseLatest =
  myoParse def
