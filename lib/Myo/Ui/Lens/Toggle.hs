module Myo.Ui.Lens.Toggle(
  envToggleOnePane,
  envToggleOneLayout,
  envOpenOnePane,
) where

import Control.Lens (mapAccumLOf)
import Control.Monad.Error.Class (MonadError, liftEither)
import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Chiasma.Ui.Data.TreeModError as TreeModError (TreeModError(..))
import Chiasma.Ui.Data.View (ViewTree)
import Chiasma.Ui.ViewTree (togglePane, toggleLayout, openPane)
import Myo.Data.Env (Env)
import Myo.Ui.View (envTreesLens)

-- map over all 'ViewTree's in the 'Env'
-- toggle a view in each tree with a function that errors if there is not exactly one view with the target 'Ident'
-- fold over the results and error if not exactly one 'ViewTree' was successful
-- if the 'Env' is empty, 'NoTrees' will be returned
envToggleOneView ::
  MonadError TreeModError m =>
  (Ident -> ViewTree -> Either TreeModError ViewTree) ->
  (Ident -> Int -> TreeModError) ->
  Ident ->
  Env ->
  m Env
envToggleOneView toggle consError ident env =
  liftEither (newEnv <$ check)
  where
    ((_, check), newEnv) = mapAccumLOf envTreesLens toggleExactlyOne (False, Left TreeModError.NoTrees) env
    toggleExactlyOne acc a =
      case (acc, toggle ident a) of
        ((False, Left err), Right tree) -> ((True, checkPreviousError err), tree)
        ((True, Left err), _) -> ((True, checkPreviousError err), a)
        ((True, _), Right _) -> ((True, Left $ consError ident 2), a)
        ((found, _), Left err) -> ((found, Left err), a)
        ((False, Right _), Right _) -> ((True, Left $ consError ident 2), a)
    checkPreviousError err = case err of
      TreeModError.NoTrees -> Right ()
      TreeModError.LayoutMissing _ -> Right ()
      TreeModError.PaneMissing _ -> Right ()
      _ -> Left err

envToggleOnePane :: MonadError TreeModError m => Ident -> Env -> m Env
envToggleOnePane =
  envToggleOneView togglePane TreeModError.AmbiguousPane

envToggleOneLayout :: MonadError TreeModError m => Ident -> Env -> m Env
envToggleOneLayout =
  envToggleOneView toggleLayout TreeModError.AmbiguousLayout

envOpenOnePane :: MonadError TreeModError m => Ident -> Env -> m Env
envOpenOnePane =
  envToggleOneView openPane TreeModError.AmbiguousPane
