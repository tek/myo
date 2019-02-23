module Myo.Ui.Data.PaneOutput(
  PaneOutput(..),
) where

import Chiasma.Data.Ident (Ident)
import Data.ByteString (ByteString)

data PaneOutput =
  PaneOutput {
    outputPaneIdent :: Ident,
    outputBytes :: ByteString
  }
  deriving (Eq, Show)
