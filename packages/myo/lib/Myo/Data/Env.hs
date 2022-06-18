module Myo.Data.Env where

import qualified Chronos

newtype Env =
  Env {
    -- runners :: [Runner],
    lastSave :: Chronos.Time
  }
  deriving stock (Show)

instance Default Env where
  def = Env (Chronos.Time 0)
