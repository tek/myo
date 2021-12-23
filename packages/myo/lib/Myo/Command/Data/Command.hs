module Myo.Command.Data.Command where

import Chiasma.Data.Ident (Identifiable (..))
import Data.Aeson (FromJSON, ToJSON (toEncoding), defaultOptions, genericToEncoding)
import Prelude hiding (lines)
import Prettyprinter (Pretty (..), nest, vsep, (<+>))

import Myo.Command.Data.CommandInterpreter (CommandInterpreter)
import Myo.Orphans ()

newtype CommandLanguage =
  CommandLanguage Text
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (MsgpackDecode, MsgpackEncode, IsString)

instance ToJSON CommandLanguage where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON CommandLanguage

data Command =
  Command {
    _interpreter :: CommandInterpreter,
    _ident :: Ident,
    _lines :: [Text],
    _runner :: Maybe Ident,
    _lang :: Maybe CommandLanguage,
    _displayName :: Maybe Text,
    _skipHistory :: Bool,
    _kill :: Bool,
    _capture :: Bool
  }
  deriving (Eq, Show, Generic)

makeClassy ''Command

instance Identifiable Command where
  identify = _ident

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

instance ToJSON Command where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Command
