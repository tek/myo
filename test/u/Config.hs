module Config(
  vars,
) where

import Ribosome.Test.Embed (Vars(..))

vars :: IO Vars
vars =
  return $ Vars []
