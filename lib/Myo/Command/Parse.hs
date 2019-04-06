{-# LANGUAGE QuasiQuotes #-}

module Myo.Command.Parse where

import Chiasma.Data.Ident (Ident)
import qualified Control.Lens as Lens (at, over)
import Control.Monad (liftM, when, (<=<))
import Control.Monad.DeepError (MonadDeepError(throwHoist), hoistEither, hoistMaybe)
import Control.Monad.DeepState (MonadDeepState, getsL, modify, setL)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.UTF8 as ByteString (toString)
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Nvim.Api.RpcCall (RpcError)
import Text.RE.PCRE.String (RE, SearchReplace, compileSearchReplace, ed, (*=~/))

import Myo.Command.Command (commandByIdent, latestCommand)
import Myo.Command.Data.Command (Command(Command, cmdIdent), CommandLanguage)
import Myo.Command.Data.CommandError (CommandError)
import qualified Myo.Command.Data.CommandLog as CommandLog (CommandLog(_current))
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (outputHandlers, parsedOutput)
import Myo.Command.Data.ParseOptions (ParseOptions(ParseOptions))
import Myo.Command.Log (commandLog)
import Myo.Command.Output (renderParseResult)
import qualified Myo.Log as Log
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(NoLang, NoHandler, NoOutput))
import Myo.Output.Data.OutputHandler (OutputHandler(OutputHandler))
import Myo.Output.Data.OutputParser (OutputParser(OutputParser))
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import qualified Myo.Settings as Settings (displayResult)

selectCommand ::
  MonadDeepError e OutputError m =>
  MonadDeepError e CommandError m =>
  MonadDeepState s CommandState m =>
  Maybe Ident ->
  m Command
selectCommand (Just ident) = commandByIdent ident
selectCommand Nothing = latestCommand

removeTerminalCodesRE :: SearchReplace RE String
removeTerminalCodesRE =
  [ed|\e\[[0-9;?]*[a-zA-z]///|]

removeLineFeedRE :: SearchReplace RE String
removeLineFeedRE =
  [ed|\r///|]

sanitizeOutput :: String -> String
sanitizeOutput =
  (*=~/ removeTerminalCodesRE) . (*=~/ removeLineFeedRE)

commandOutput ::
  MonadDeepError e OutputError m =>
  MonadDeepState s CommandState m =>
  Ident ->
  m Text
commandOutput ident = do
  clog <- commandLog ident
  maybe (throwHoist (OutputError.NoOutput ident)) convert clog
  where
    convert = return . Text.pack . sanitizeOutput . ByteString.toString . CommandLog._current

handlersForLang ::
  (MonadDeepError e OutputError m, MonadDeepState s CommandState m) =>
  CommandLanguage ->
  m [OutputHandler]
handlersForLang lang = do
  result <- getsL @CommandState $ CommandState.outputHandlers . Lens.at lang
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
    parse (OutputHandler parser) = parseWith parser output

parseCommand ::
  (MonadRibo m, MonadIO m, MonadDeepError e OutputError m, MonadDeepState s CommandState m) =>
  Command ->
  m [ParsedOutput]
parseCommand (Command _ ident _ _ (Just lang)) = do
  output <- commandOutput ident
  Log.debug (show output)
  parseWithLang lang output
parseCommand (Command _ ident _ _ _) =
  throwHoist $ OutputError.NoLang ident

myoParse ::
  MonadDeepError e CommandError m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e RpcError m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s CommandState m =>
  MonadIO m =>
  MonadRibo m =>
  Nvim m =>
  ParseOptions ->
  m ()
myoParse (ParseOptions _ ident _) = do
  cmd <- selectCommand ident
  parsedOutput <- parseCommand cmd
  setL @CommandState CommandState.parsedOutput (Just parsedOutput)
  display <- setting Settings.displayResult
  when display $ renderParseResult (cmdIdent cmd) parsedOutput

myoParseLatest ::
  MonadDeepError e CommandError m =>
  MonadDeepError e OutputError m =>
  MonadDeepError e RpcError m =>
  MonadDeepError e SettingError m =>
  MonadDeepState s CommandState m =>
  MonadIO m =>
  MonadRibo m =>
  Nvim m =>
  m ()
myoParseLatest =
  myoParse (ParseOptions Nothing Nothing Nothing)

addHandler :: MonadDeepState s CommandState m => CommandLanguage -> OutputHandler -> m ()
addHandler lang parser =
  modify @CommandState $ Lens.over (CommandState.outputHandlers . Lens.at lang) update
  where
    update (Just current) = Just (parser : current)
    update Nothing = Just [parser]
