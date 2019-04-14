module Myo.Command.Data.Command where

import Chiasma.Data.Ident (Ident, Identifiable(..))
import Chiasma.Data.Text.Pretty (prettyS)
import Data.Maybe (maybeToList)
import Data.Text.Prettyprint.Doc (Pretty(..), nest, vsep, (<+>))
import GHC.Generics (Generic)
import Ribosome.Msgpack.Decode (MsgpackDecode(..))

import Myo.Command.Data.CommandInterpreter (CommandInterpreter)

newtype CommandLanguage =
  CommandLanguage Text
  deriving (Eq, Show, Ord, Generic, MsgpackDecode)

data Command =
  Command {
    cmdInterpreter :: CommandInterpreter,
    cmdIdent :: Ident,
    cmdLines :: [Text],
    cmdRunner :: Maybe Ident,
    lang :: Maybe CommandLanguage
  }
  deriving (Eq, Show)

instance Identifiable Command where
  identify = cmdIdent

instance Pretty Command where
  pretty (Command iprt ident lines' runner lang) =
    nest 2 . vsep $ header : info
    where
      header = prettyS "*" <+> pretty ident
      info = prettyIprt : prettyLines : maybeToList (prettyRunner <$> runner) <> maybeToList (prettyLang <$> lang)
      prettyIprt = prettyS "interpreter:" <+> pretty iprt
      prettyRunner _ = prettyS "runner"
      prettyLines = nest 2 . vsep $ prettyS "lines:" : (pretty <$> lines')
      prettyLang (CommandLanguage a) = prettyS "language:" <+> pretty a
