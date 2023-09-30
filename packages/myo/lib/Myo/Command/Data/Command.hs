module Myo.Command.Data.Command where

import qualified Chiasma.Data.Ident as Ident
import Chiasma.Data.Ident (Ident)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Exon (exon)
import Prelude hiding (lines)
import Prettyprinter (Pretty (..), nest, vsep, (<+>))
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.CommandInterpreter (CommandInterpreter)
import qualified Myo.Command.Data.CommandSpec
import Myo.Command.Data.CommandSpec (CommandSpec, parseCommandSpec')
import Myo.Command.Data.CommandTemplate (renderTemplate)
import Myo.Command.Data.Param (ParamDefault, ParamId, renderParamDefault)
import Myo.Data.CommandId (CommandId (CommandId))

newtype CommandLanguage =
  CommandLanguage Text
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (MsgpackDecode, MsgpackEncode, IsString)

json ''CommandLanguage

data Command =
  Command {
    interpreter :: CommandInterpreter,
    ident :: CommandId,
    cmdLines :: CommandSpec,
    runner :: Maybe Ident,
    lang :: Maybe CommandLanguage,
    displayName :: Maybe Text,
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
          prettyRunner <$> runner,
          prettyLang <$> lang,
          prettyName <$> displayName,
          Just $ "skip history:" <+> pretty skipHistory,
          Just $ "kill" <+> pretty kill,
          Just $ "capture:" <+> pretty capture
          ]

      prettyIprt = "interpreter:" <+> pretty interpreter
      prettyRunner r = "runner:" <+> pretty r
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
    runner = Nothing,
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

shortIdent :: CommandId -> Text
shortIdent = \case
  CommandId (Ident.Str n) -> n
  CommandId (Ident.Uuid i) -> Text.take 6 (show i)

name :: Command -> Text
name Command {..} =
  fromMaybe (shortIdent ident) displayName
