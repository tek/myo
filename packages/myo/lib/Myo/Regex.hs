module Myo.Regex where

import Control.Lens.Regex.Text (mkRegexTraversalQQ)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Text.Regex.PCRE.Light (multiline)

regexML :: QuasiQuoter
regexML =
  mkRegexTraversalQQ [multiline]
