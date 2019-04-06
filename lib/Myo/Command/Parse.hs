module Myo.Command.Parse where

import Chiasma.Data.Ident (Ident)
import qualified Control.Lens as Lens (at, over)
import Control.Monad (when)
import Control.Monad.DeepError (hoistEither, hoistMaybe, MonadDeepError(throwHoist))
import Control.Monad.DeepState (getsL, modify, MonadDeepState, setL)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Nvim.Api.RpcCall (RpcError)

import Myo.Command.Command (commandByIdent, latestCommand)
import Myo.Command.Data.Command (Command(Command, cmdIdent), CommandLanguage)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState)
import qualified Myo.Command.Data.CommandState as CommandState (outputHandlers, parsedOutput)
import Myo.Command.Data.ParseOptions (ParseOptions(ParseOptions))
import Myo.Command.Output (renderParseResult)
import Myo.Output.Data.OutputError (OutputError)
import qualified Myo.Output.Data.OutputError as OutputError (OutputError(NoLang, NoHandler))
import Myo.Output.Data.OutputHandler (OutputHandler(OutputHandler))
import Myo.Output.Data.OutputParser (OutputParser(OutputParser))
import Myo.Output.Data.ParsedOutput (ParsedOutput)
import qualified Myo.Settings as Settings (displayResult)

selectCommand ::
  (MonadDeepError e OutputError m, MonadDeepError e CommandError m, MonadDeepState s CommandState m) =>
  Maybe Ident ->
  m Command
selectCommand (Just ident) = commandByIdent ident
selectCommand Nothing = latestCommand

commandOutput :: MonadDeepState s CommandState m => Ident -> m Text
commandOutput _ =
  return ""

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
  (MonadDeepError e OutputError m, MonadDeepState s CommandState m) =>
  Command ->
  m [ParsedOutput]
parseCommand (Command _ ident _ _ (Just lang)) = do
  output <- commandOutput ident
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

addHandler :: MonadDeepState s CommandState m => CommandLanguage -> OutputHandler -> m ()
addHandler lang parser =
  modify @CommandState $ Lens.over (CommandState.outputHandlers . Lens.at lang) update
  where
    update (Just current) = Just (parser : current)
    update Nothing = Just [parser]
