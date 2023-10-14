module Myo.Command.Override where

import qualified Chiasma.Data.Ident as Ident
import Data.Char (isAlphaNum)
import qualified Data.Text as Text
import Exon (exon)
import Path (stripProperPrefix)
import Ribosome (MsgpackDecode, MsgpackEncode (..), Rpc, RpcError)
import Ribosome.Api (currentBufferPath, currentCursor, nvimBufGetLines, nvimCallFunction, nvimCwd, nvimGetCurrentBuf)
import Ribosome.Host.Path (pathText)

import Myo.Api.Function (callIfExists)
import qualified Myo.Command.Data.Command as Command
import Myo.Command.Data.Command (Command, CommandLanguage)
import qualified Myo.Command.Data.CommandError as CommandError
import Myo.Command.Data.CommandError (CommandError)
import Myo.Command.Data.CommandInterpreter (CommandInterpreter (Shell, System))
import Myo.Command.Data.CommandSpec (parseCommandSpec)
import Myo.Command.Data.CommandTemplate (CommandTemplate)
import Myo.Command.Data.Param (ParamValues)
import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Data.CommandId (CommandId (CommandId))
import Myo.Data.CommandName (CommandName (CommandName))
import Myo.Data.CommandQuery (queryId)
import qualified Myo.Effect.Commands as Commands
import Myo.Effect.Commands (Commands)

data TestPosition =
  TestPosition {
    path :: Text,
    relative :: Text,
    line :: Int,
    col :: Int,
    text :: Text,
    cword :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

data Overrides =
  Overrides {
    command :: Maybe CommandId,
    lines :: Maybe CommandTemplate,
    params :: Maybe ParamValues,
    shell :: Maybe CommandId,
    target :: Maybe UiTarget,
    lang :: Maybe CommandLanguage,
    displayName :: Maybe CommandName,
    skipHistory :: Maybe Bool,
    kill :: Maybe Bool,
    capture :: Maybe Bool,
    maxLogBytes :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (MsgpackDecode, MsgpackEncode)

override :: Traversal' Command a -> Maybe a -> Command -> Command
override lens = \case
  Just a -> lens .~ a
  Nothing -> id

overrideMaybe :: Traversal' Command (Maybe a) -> Maybe a -> Command -> Command
overrideMaybe lens = \case
  Just a -> lens ?~ a
  Nothing -> id

withOverrides :: Overrides -> Command -> Command
withOverrides overrides =
  override (#cmdLines . #template) overrides.lines
  .
  override #interpreter (System . Just <$> overrides.target)
  .
  override #interpreter (Shell <$> overrides.shell)
  .
  overrideMaybe #lang overrides.lang
  .
  overrideMaybe #displayName overrides.displayName
  .
  override #skipHistory overrides.skipHistory
  .
  override #kill overrides.kill
  .
  override #capture overrides.capture
  .
  overrideMaybe #maxLogBytes overrides.maxLogBytes

assembleOverriddenCommand ::
  Member Commands r =>
  Sem r Command ->
  Overrides ->
  Sem r Command
assembleOverriddenCommand defaultBase overrides = do
  base <- fromMaybeA defaultBase =<< traverse (Commands.query . queryId) overrides.command
  pure (withOverrides overrides base)

queryOverrides ::
  Members [Commands, Stop CommandError, Rpc !! RpcError, Rpc] r =>
  Sem r Command ->
  Text ->
  Sem r (Command, ParamValues)
queryOverrides base callback = do
  cwd <- nvimCwd
  path <- currentBufferPath
  (lno, col) <- currentCursor
  (text, cword) <- fold <$> resumeAs @RpcError Nothing do
    buf <- nvimGetCurrentBuf
    bufLine <- head <$> nvimBufGetLines buf lno (lno + 1) False
    cword <- nvimCallFunction "expand" [toMsgpack ("<cword>" :: Text)]
    pure (Just (fold bufLine, cword))
  let
    rel = foldMap pathText (stripProperPrefix cwd =<< path)
    pos = TestPosition (maybe (pathText cwd) pathText path) rel lno col text cword
  callIfExists callback [toMsgpack pos] >>= \case
    Just (Right overrides) -> do
      cmd <- assembleOverriddenCommand base overrides
      pure (cmd, fold overrides.params)
    Just (Left line) -> do
      spec <- stopEitherWith (CommandError.InvalidTemplate False line) (parseCommandSpec (Left line))
      cmd <- base
      pure (cmd & #cmdLines .~ spec, mempty)
    Nothing -> do
      cmd <- base
      pure (cmd, mempty)

overridesCallbackSuffix :: Maybe CommandName -> CommandId -> Maybe Text
overridesCallbackSuffix (Just (CommandName name)) _ = Just name
overridesCallbackSuffix Nothing (CommandId (Ident.Str name)) = Just name
overridesCallbackSuffix Nothing (CommandId (Ident.Uuid _)) = Nothing

escapeForNvim :: Char -> Char
escapeForNvim c
  | isAlphaNum c || c == '_'
  = c
  | otherwise
  = '_'

overridesCallbackForId :: Maybe CommandName -> CommandId -> Maybe Text
overridesCallbackForId displayName cid = do
  name <- overridesCallbackSuffix displayName cid
  pure [exon|MyoOverrides_#{Text.map escapeForNvim name}|]

queryRegularOverrides ::
  Members [Commands, Stop CommandError, Rpc !! RpcError, Rpc] r =>
  Command ->
  Sem r (Command, ParamValues)
queryRegularOverrides base = do
  case overridesCallbackForId base.displayName base.ident of
    Just callback -> queryOverrides (pure base) callback
    Nothing -> pure (base, mempty)
