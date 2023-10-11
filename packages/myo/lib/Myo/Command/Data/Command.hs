module Myo.Command.Data.Command where

import qualified Chiasma.Data.Ident as Ident
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Exon (exon)
import Prelude hiding (lines)
import Prettyprinter (Pretty (..), nest, vsep, (<+>))
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.CommandInterpreter (CommandInterpreter (..))
import qualified Myo.Command.Data.CommandSpec
import Myo.Command.Data.CommandSpec (CommandSpec, parseCommandSpec')
import Myo.Command.Data.CommandTemplate (renderTemplate)
import Myo.Command.Data.Param (ParamDefault, ParamDefaults, ParamId, renderParamDefault)
import Myo.Command.Data.UiTarget (UiTarget)
import Myo.Data.CommandId (CommandId (CommandId), commandIdText)
import Myo.Data.CommandName (CommandName (CommandName))

newtype CommandLanguage =
  CommandLanguage Text
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (MsgpackDecode, MsgpackEncode, IsString)

json ''CommandLanguage

-- TODO commandShell should probably be in CommandInterpreter.Shell
data Command =
  Command {
    interpreter :: CommandInterpreter,
    ident :: CommandId,
    cmdLines :: CommandSpec,
    lang :: Maybe CommandLanguage,
    displayName :: Maybe CommandName,
    skipHistory :: Bool,
    kill :: Bool,
    capture :: Bool,
    maxLogBytes :: Maybe Int,
    commandShell :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Pretty Command where
  pretty (Command {..}) =
    nest 2 . vsep $ header : info
    where
      header = "*" <+> pretty ident

      info = prettyIprt : prettyLines : prettyParams : opt

      opt =
        catMaybes [
          prettyLang <$> lang,
          prettyName <$> displayName,
          Just $ "skip history:" <+> pretty skipHistory,
          Just $ "kill" <+> pretty kill,
          Just $ "capture:" <+> pretty capture
          ]

      prettyIprt = "interpreter:" <+> pretty interpreter
      prettyLines = nest 2 . vsep $ "lines:" : (pretty <$> renderTemplate cmdLines.template)
      prettyParams = nest 2 (vsep ("params:" : (pretty . renderParam <$> Map.toList cmdLines.params)))
      prettyLang (CommandLanguage a) = "language:" <+> pretty a
      prettyName n = "name:" <+> pretty n

      renderParam :: (ParamId, ParamDefault) -> Text
      renderParam (k, v) = [exon|##{k}: ##{renderParamDefault v}|]

json ''Command

consSpec :: CommandInterpreter -> CommandId -> CommandSpec -> Command
consSpec interpreter ident cmdLines =
  Command {
    cmdLines,
    lang = Nothing,
    displayName = Nothing,
    skipHistory = False,
    kill = False,
    capture = False,
    maxLogBytes = Nothing,
    commandShell = False,
    ..
  }

cons :: CommandInterpreter -> CommandId -> [Text] -> Command
cons interpreter ident cmdLines =
  consSpec interpreter ident (parseCommandSpec' cmdLines)

withParams :: ParamDefaults -> Command -> Command
withParams defs =
  #cmdLines . #params .~ defs

shortIdent :: CommandId -> Text
shortIdent = \case
  CommandId (Ident.Str n) -> n
  CommandId (Ident.Uuid i) -> Text.take 6 (show i)

describe :: Command -> Text
describe Command {..} =
  maybe (shortIdent ident) coerce displayName

commandHasId :: CommandId -> Command -> Bool
commandHasId target Command {ident} = ident == target

commandHasName :: CommandName -> Command -> Bool
commandHasName target = \case
  Command {displayName = Just name} -> name == target
  _ -> False

commandHasAny :: Text -> Command -> Bool
commandHasAny target = \case
  Command {ident} | commandIdText ident == target -> True
  Command {displayName = Just (CommandName name)} | name == target -> True
  _ -> False

systemCommand ::
  Maybe UiTarget ->
  CommandId ->
  CommandSpec ->
  Command
systemCommand target =
  consSpec (System target)

shellCommand ::
  CommandId ->
  CommandId ->
  CommandSpec ->
  Command
shellCommand target =
  consSpec (Shell target)
