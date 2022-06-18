module Myo.Command.Data.Command where

import Chiasma.Data.Ident (Ident, Identifiable (..))
import Polysemy.Time.Json (json)
import Prelude hiding (lines)
import Prettyprinter (Pretty (..), nest, vsep, (<+>))
import Ribosome (MsgpackDecode, MsgpackEncode)

import Myo.Command.Data.CommandInterpreter (CommandInterpreter)
import Myo.Orphans ()

newtype CommandLanguage =
  CommandLanguage Text
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (MsgpackDecode, MsgpackEncode, IsString)

json ''CommandLanguage

data Command =
  Command {
    interpreter :: CommandInterpreter,
    ident :: Ident,
    lines :: [Text],
    runner :: Maybe Ident,
    lang :: Maybe CommandLanguage,
    displayName :: Maybe Text,
    skipHistory :: Bool,
    kill :: Bool,
    capture :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance Identifiable Command where
  identify =
    ident

instance Pretty Command where
  pretty (Command iprt ident' lines' runner' lang' displayName' skipHistory' kill' capture') =
    nest 2 . vsep $ header : info
    where
      header =
        "*" <+> pretty ident'
      info =
        prettyIprt : prettyLines : opt
      opt =
        catMaybes [
          prettyRunner <$> runner',
          prettyLang <$> lang',
          prettyName <$> displayName',
          Just $ "skip history:" <+> pretty skipHistory',
          Just $ "kill" <+> pretty kill',
          Just $ "capture:" <+> pretty capture'
          ]
      prettyIprt = "interpreter:" <+> pretty iprt
      prettyRunner r = "runner:" <+> pretty r
      prettyLines = nest 2 . vsep $ "lines:" : (pretty <$> lines')
      prettyLang (CommandLanguage a) = "language:" <+> pretty a
      prettyName n = "name:" <+> pretty n

json ''Command
