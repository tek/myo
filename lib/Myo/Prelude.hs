{-# LANGUAGE NoImplicitPrelude #-}

module Myo.Prelude (
  module Ribosome.PreludeExport,
  module Chiasma.Data.Ident,
  qt,
) where

import Chiasma.Data.Ident (Ident, identText)
import Language.Haskell.TH.Quote (QuasiQuoter)
import NeatInterpolation (trimming)
import Ribosome.PreludeExport

qt :: QuasiQuoter
qt =
  trimming
{-# INLINE qt #-}
