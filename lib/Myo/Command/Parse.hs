{-# LANGUAGE QuasiQuotes #-}

module Myo.Command.Parse where

import Chiasma.Data.Ident (Ident)
import Control.Lens (at, element, firstOf, over, view)
import Ribosome.Config.Setting (setting, settingMaybe)
import Ribosome.Data.SettingError (SettingError)
import qualified Ribosome.Log as Log
import Ribosome.Msgpack.Error (DecodeError)
import Text.RE.PCRE.Text (RE, SearchReplace, ed, (*=~/))

import Myo.Command.Command (commandByIdent, latestCommand)
import Myo.Command.Data.Command (Command(Command), CommandLanguage(CommandLanguage))
import qualified Myo.Command.Data.Command as Command (ident)
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandLog as CommandLog (current, previous)
import Myo.Command.Data.CommandState (CommandState, OutputState(OutputState))
import qualified Myo.Command.Data.CommandState as CommandState (output, outputHandlers)
import Myo.Command.Data.ParseOptions (ParseOptions(ParseOptions))
import Myo.Command.History (displayNameByIdent)
import Myo.Command.Log (commandLog, commandLogByName)
import Myo.Command.Output (compileAndRenderReport)
import qualified Myo.Output.Data.EventIndex as EventIndex (Absolute(Absolute))
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(NoLang, NoHandler, NoOutput))
import Myo.Output.Data.OutputHandler (OutputHandler(OutputHandler))
import Myo.Output.Data.OutputParser (OutputParser(OutputParser))
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import qualified Myo.Output.Data.ParsedOutput as ParsedOutput (allEmpty, events, syntax)
import qualified Myo.Settings as Settings (displayResult, proteomeMainType)

selectCommand ::
  MonadDeepError e OutputError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  Maybe Ident ->
  m Command
selectCommand (Just ident) = commandByIdent "selectCommand" ident
selectCommand Nothing = latestCommand

removeTerminalCodesRE :: SearchReplace RE Text
removeTerminalCodesRE =
  [ed|\e\[[0-9;?]*[a-zA-z]///|]

removeLineFeedRE :: SearchReplace RE Text
removeLineFeedRE =
  [ed|\r///|]

sanitizeOutput :: Text -> Text
sanitizeOutput =
  (*=~/ removeTerminalCodesRE) . (*=~/ removeLineFeedRE)

commandOutputResult ::
  MonadDeepError e OutputError m =>
  Text ->
  Maybe ByteString ->
  m Text
commandOutputResult ident =
  maybe err convert
  where
    convert =
      return . sanitizeOutput . decodeUtf8
    err =
      throwHoist $ OutputError.NoOutput ident

commandOutput ::
  MonadDeepError e OutputError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  Ident ->
  Maybe Natural ->
  m Text
commandOutput ident index = do
  name <- displayNameByIdent ident
  commandOutputResult name . (>>= select) =<< commandLog ident
  where
    select =
      maybe (Just . view CommandLog.current) selectPrevious (fromIntegral <$> index)
    selectPrevious i =
      firstOf (CommandLog.previous . element i)

commandOutputByName ::
  MonadDeepError e OutputError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  Text ->
  m Text
commandOutputByName name =
  commandOutputResult name =<< view CommandLog.current <$$> commandLogByName name

handlersForLang ::
  (MonadDeepError e OutputError m, MonadDeepState s CommandState m) =>
  CommandLanguage ->
  m [OutputHandler]
handlersForLang lang = do
  result <- getL @CommandState $ CommandState.outputHandlers . at lang
  hoistMaybe (OutputError.NoHandler lang) result

parseWith :: MonadDeepError s OutputError m => OutputParser -> Text -> m ParsedOutput
parseWith (OutputParser parser) =
  hoistEither . parser

parseWithLang ::
  (MonadDeepError e OutputError m, MonadDeepState s CommandState m) =>
  CommandLanguage ->
  Text ->
  m [ParsedOutput]
parseWithLang lang output = do
  handlers <- handlersForLang lang
  traverse parse handlers
  where
    parse (OutputHandler parser) =
      parseWith parser output

parseCommandWithLang ::
  MonadRibo m =>
  MonadIO m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  CommandLanguage ->
  Ident ->
  m [ParsedOutput]
parseCommandWithLang lang ident =
  ensureEvents =<< parseIndex Nothing
  where
    ensureEvents a =
      if ParsedOutput.allEmpty a then parseIndex (Just 0) else pure a
    parseIndex index = do
      output <- commandOutput ident index
      Log.showDebug "parse output:" output
      parseWithLang lang output

projectLanguage ::
  NvimE e m =>
  MonadRibo m =>
  m (Maybe CommandLanguage)
projectLanguage =
  CommandLanguage <$$> settingMaybe Settings.proteomeMainType

parseCommand ::
  NvimE e m =>
  MonadRibo m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  Command ->
  m [ParsedOutput]
parseCommand (Command _ ident _ _ (Just lang) _ _) =
  parseCommandWithLang lang ident
parseCommand (Command _ ident _ _ Nothing _ _) = do
  lang <- hoistMaybe (OutputError.NoLang ident) =<< projectLanguage
  parseCommandWithLang lang ident

storeParseResult ::
  MonadDeepState s CommandState m =>
  Ident ->
  [ParsedOutput] ->
  m ()
storeParseResult ident output =
  setL @CommandState CommandState.output (Just outputState)
  where
    outputState =
      OutputState ident syntax events (EventIndex.Absolute 0) Nothing
    syntax =
      view ParsedOutput.syntax <$> output
    events =
      foldMap (view ParsedOutput.events) output

myoParse ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s CommandState m =>
  ParseOptions ->
  m ()
myoParse (ParseOptions _ ident _) = do
  cmd <- selectCommand ident
  parsedOutput <- parseCommand cmd
  storeParseResult (view Command.ident cmd) parsedOutput
  display <- setting Settings.displayResult
  when display compileAndRenderReport

myoParseLatest ::
  NvimE e m =>
  MonadRibo m =>
  MonadBaseControl IO m =>
  MonadDeepError e CommandError m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e DecodeError m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s CommandState m =>
  m ()
myoParseLatest =
  myoParse (ParseOptions Nothing Nothing Nothing)

addHandler :: MonadDeepState s CommandState m => CommandLanguage -> OutputHandler -> m ()
addHandler lang parser =
  modify @CommandState $ over (CommandState.outputHandlers . at lang) update
  where
    update (Just current) = Just (parser : current)
    update Nothing = Just [parser]
