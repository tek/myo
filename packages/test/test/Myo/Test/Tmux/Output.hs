module Myo.Test.Tmux.Output where

import qualified Data.Text as Text
import Polysemy.Test (Hedgehog, assert)

cleanLine :: Text -> Text
cleanLine l =
  Text.strip (fromMaybe l (Text.stripPrefix "$ " l))

cleanLines :: [Text] -> [Text]
cleanLines =
  fmap cleanLine

containsLine ::
  HasCallStack =>
  Member (Hedgehog IO) r =>
  Text ->
  [Text] ->
  Sem r ()
containsLine target ls =
  withFrozenCallStack do
    assert (any (Text.isSuffixOf target) (cleanLines ls))
