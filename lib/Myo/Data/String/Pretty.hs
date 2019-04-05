module Myo.Data.String.Pretty where

import Chiasma.Data.Text.Pretty (prettyS)
import Data.Text.Prettyprint.Doc (Doc)

backtick :: Doc a -> Doc a
backtick a =
  prettyS "`" <> a <> "`"
