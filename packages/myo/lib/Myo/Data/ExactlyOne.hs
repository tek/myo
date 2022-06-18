module Myo.Data.ExactlyOne where

data ExactlyOne a =
  NotFound a
  |
  Multiple
  |
  One a
  deriving stock (Eq, Show, Functor)

foldForOne :: (a -> ExactlyOne a) -> [a] -> ExactlyOne [a]
foldForOne f =
  foldl folder (NotFound [])
  where
    folder (NotFound acc) a =
      (: acc) <$> f a
    folder (One acc) a =
      case f a of
        NotFound a' -> One (a' : acc)
        _ -> Multiple
    folder Multiple _ =
      Multiple
