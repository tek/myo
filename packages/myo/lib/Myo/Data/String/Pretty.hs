module Myo.Data.String.Pretty where

import Data.Text.Prettyprint.Doc (Doc)

backtick :: Doc a -> Doc a
backtick a =
  "`" <> a <> "`"
