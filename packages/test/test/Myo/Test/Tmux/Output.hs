module Myo.Test.Tmux.Output where

import qualified Data.Text as Text
import Hedgehog (TestT, (===))

containsLine ::
  Monad m =>
  HasCallStack =>
  Text ->
  [Text] ->
  TestT m ()
containsLine target ls =
  withFrozenCallStack do
    True === any (Text.isSuffixOf target) ls
