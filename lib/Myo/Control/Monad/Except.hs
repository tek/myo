module Myo.Control.Monad.Except(
  coalesceE,
) where

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Trans.Except (ExceptT, catchE)

coalesceE :: Monad m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
coalesceE trans ma =
  catchE ma trans'
  where
    trans' = liftEither . Left . trans
