module Myo.Command.Data.Command where

import qualified Chiasma.Data.Ident as Ident
import Chiasma.Data.Ident (Ident)
import qualified Data.Text as Text
import Prelude hiding (lines)
import Prettyprinter (Pretty (..), nest, vsep, (<+>))
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.CommandInterpreter (CommandInterpreter)
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
    cmdLines :: [Text],
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
      header =
        "*" <+> pretty ident
      info =
        prettyIprt : prettyLines : opt
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
      prettyLines = nest 2 . vsep $ "lines:" : (pretty <$> cmdLines)
      prettyLang (CommandLanguage a) = "language:" <+> pretty a
      prettyName n = "name:" <+> pretty n

json ''Command

cons :: CommandInterpreter -> CommandId -> [Text] -> Command
cons interpreter ident cmdLines =
  Command {
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

shortIdent :: CommandId -> Text
shortIdent = \case
  CommandId (Ident.Str n) -> n
  CommandId (Ident.Uuid i) -> Text.take 6 (show i)

name :: Command -> Text
name Command {..} =
  fromMaybe (shortIdent ident) displayName
