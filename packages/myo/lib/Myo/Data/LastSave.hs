module Myo.Data.LastSave where

import qualified Chronos

newtype LastSave =
  LastSave { unLastSave :: Chronos.Time }
  deriving stock (Eq, Show)

instance Default LastSave where
  def =
    LastSave (Chronos.Time 0)
