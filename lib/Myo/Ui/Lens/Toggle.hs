module Myo.Ui.Lens.Toggle(
  envToggleOnePane,
) where

import Control.Lens (mapAccumLOf)
import Control.Monad.Error.Class (MonadError, liftEither)
import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Chiasma.Ui.Data.TreeModError as TreeModError (TreeModError(..))
import Chiasma.Ui.ViewTree (togglePane)
import Myo.Data.Env (Env)
import Myo.Ui.View (envTreesLens)

envToggleOnePane :: MonadError TreeModError m => Ident -> Env -> m Env
envToggleOnePane ident env =
  liftEither (newEnv <$ check)
  where
    (check, newEnv) = mapAccumLOf envTreesLens toggleExactlyOnePane (Left TreeModError.NoTrees) env
    toggleExactlyOnePane acc a =
      case (acc, togglePane ident a) of
        (Left TreeModError.NoTrees, Right tree) -> (Right (), tree)
        (Left err, _) -> (Left err, a)
        (Right _, Right _) -> (Left $ TreeModError.AmbiguousPane ident 2, a)
        (Right _, Left err) -> (Left err, a)
