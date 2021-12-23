module Myo.Data.String.Pretty where

import Prettyprinter (Doc)

backtick :: Doc a -> Doc a
backtick a =
  "`" <> a <> "`"
