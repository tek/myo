module Myo.Loop where

useWhileJust ::
  Sem r (Maybe a) ->
  (a -> Sem r ()) ->
  Sem r ()
useWhileJust acquire use =
  spin
  where
    spin =
      acquire >>= \case
        Just !a -> do
          use a
          spin
        Nothing -> unit
