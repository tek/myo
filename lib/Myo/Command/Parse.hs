module Myo.Command.Parse(
  myoParse,
  addParser,
) where

import Chiasma.Data.Ident (Ident)
import qualified Control.Lens as Lens (over)
import Control.Monad (when)
import Control.Monad.DeepError (MonadDeepError(throwHoist))
import Control.Monad.DeepState (MonadDeepState, modify, setL)
import Control.Monad.IO.Class (MonadIO)
import Data.Map ((!?))
import qualified Data.Map as Map (insert)
import Data.Maybe (fromMaybe)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Monad.Ribo (MonadRibo, Nvim)
import Ribosome.Data.SettingError (SettingError)
import Ribosome.Nvim.Api.RpcCall (RpcError)

import Myo.Command.Command (commandByIdent, latestCommand)
import Myo.Command.Data.Command (Command(Command), CommandLanguage)
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandState (CommandState, Parser)
import qualified Myo.Command.Data.CommandState as CommandState (parseResult, parsers)
import Myo.Command.Data.OutputError (OutputError)
import qualified Myo.Command.Data.OutputError as OutputError (OutputError(NoLang))
import Myo.Command.Data.ParseOptions (ParseOptions(ParseOptions))
import Myo.Command.Data.ParsedOutput (ParsedOutput)
import Myo.Command.Output (renderParseResult)
import qualified Myo.Settings as Settings (displayResult)

selectCommand ::
  (MonadDeepError e OutputError m, MonadDeepError e CommandError m, MonadDeepState s CommandState m) =>
  Maybe Ident ->
  m Command
selectCommand (Just ident) = commandByIdent ident
selectCommand Nothing = latestCommand

commandOutput :: Ident -> m ()
commandOutput = undefined

parserForLang :: Monad m => CommandLanguage -> m ()
parserForLang = undefined

parseWithLang :: Monad m => CommandLanguage -> m ParsedOutput
parseWithLang lang = do
  _ <- parserForLang lang
  return undefined
  -- output <-

parseCommand ::
  (MonadDeepError e OutputError m, MonadDeepState s CommandState m) =>
  Command ->
  m ParsedOutput
parseCommand (Command _ ident _ _ (Just lang)) = do
  _ <- commandOutput ident
  parseWithLang lang
parseCommand (Command _ ident _ _ _) =
  throwHoist $ OutputError.NoLang ident

myoParse ::
  ∀ s e m.
  (MonadRibo m, MonadIO m, Nvim m, MonadDeepState s CommandState m, MonadDeepError e OutputError m, MonadDeepError e CommandError m, MonadDeepError e RpcError m, MonadDeepError e SettingError m) =>
  ParseOptions ->
  m ()
myoParse (ParseOptions _ ident _) = do
  cmd <- selectCommand ident
  parseResult <- parseCommand cmd
  setL @CommandState CommandState.parseResult (Just parseResult)
  display <- setting Settings.displayResult
  when display $ renderParseResult parseResult

addParser :: MonadDeepState s CommandState m => CommandLanguage -> Parser -> m ()
addParser lang parser =
  modify @CommandState $ Lens.over CommandState.parsers update
  where
    update parsers =
      Map.insert lang (parser : current) parsers
      where
        current = fromMaybe [] (parsers !? lang)
