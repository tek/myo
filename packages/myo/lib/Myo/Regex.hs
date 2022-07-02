module Myo.Regex where

import Control.Lens (IndexedTraversal')
import Control.Lens.Regex.Text (Match, mkRegexTraversalQQ, regex)
import Language.Haskell.TH.Quote (QuasiQuoter)
import Text.Regex.PCRE.Light (multiline)

regexML :: QuasiQuoter
regexML =
  mkRegexTraversalQQ [multiline]

removeControlCharsRE :: IndexedTraversal' Int Text Match
removeControlCharsRE =
  [regex|(\x{9b}|\x{1b}\[)[0-?]*[ -\/]*[@-~]|]
