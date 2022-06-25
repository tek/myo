module Myo.Loop where

import qualified Time

-- |Repeatedly run the @action@, sleeping for @interval@ between executions.
-- Stops when @action@ returns @Just a@, returning the contained @a@.
untilJust ::
  ∀ t d u r a .
  Member (Time t d) r =>
  TimeUnit u =>
  u ->
  Sem r (Maybe a) ->
  Sem r a
untilJust interval action =
  spin
  where
    spin =
      action >>= \case
        Just a -> pure a
        Nothing -> Time.sleep interval *> spin
