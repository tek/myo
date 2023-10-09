module Myo.Data.CommandName where

import Prettyprinter (Pretty (pretty))
import Ribosome (MsgpackDecode, MsgpackEncode)

newtype CommandName =
  CommandName Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord, MsgpackDecode, MsgpackEncode, ToJSON, FromJSON)

instance Pretty CommandName where
  pretty (CommandName name) = pretty name
