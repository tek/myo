module Myo.Ui.Lens.Toggle where

import Chiasma.Data.Ident (Ident)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import qualified Chiasma.Ui.Data.TreeModError as TreeModError (TreeModError(..))
import Chiasma.Ui.Data.View (ViewTree)
import Chiasma.Ui.ViewTree (openPane, toggleLayout, togglePane)
import Control.Lens (mapAccumLOf)
import Control.Monad.DeepError (MonadDeepError, hoistEither)

import Myo.Ui.Data.UiState (UiState)
import Myo.Ui.View (uiTreesLens)

-- |map over all 'ViewTree's in the 'Env'
-- toggle a view in each tree with a function that errors if there is not exactly one view with the target 'Ident'
-- fold over the results and error if not exactly one 'ViewTree' was successful
-- if the 'Env' is empty, 'NoTrees' will be returned
toggleOneView ::
  MonadDeepError e TreeModError m =>
  (Ident -> ViewTree -> Either TreeModError ViewTree) ->
  (Ident -> Int -> TreeModError) ->
  Ident ->
  UiState ->
  m UiState
toggleOneView toggle consError ident env =
  hoistEither (newEnv <$ check)
  where
    ((_, check), newEnv) = mapAccumLOf uiTreesLens toggleExactlyOne (False, Left TreeModError.NoTrees) env
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

toggleOnePane :: MonadDeepError e TreeModError m => Ident -> UiState -> m UiState
toggleOnePane =
  toggleOneView togglePane TreeModError.AmbiguousPane

toggleOneLayout :: MonadDeepError e TreeModError m => Ident -> UiState -> m UiState
toggleOneLayout =
  toggleOneView toggleLayout TreeModError.AmbiguousLayout

openOnePane :: MonadDeepError e TreeModError m => Ident -> UiState -> m UiState
openOnePane =
  toggleOneView openPane TreeModError.AmbiguousPane
