{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TogglePaneSpec(
  htf_thisModulesTests
) where

import Test.Framework
import Control.Lens (mapMOf)
import Control.Monad.Error.Class (throwError, liftEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Default.Class (Default(def))
import UnliftIO.Exception (throwString)
import qualified Chiasma.Monad.Tmux as Tmux (read)
import Chiasma.Data.Ident
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Chiasma.Codec.Data as Codec (Pane)
import Chiasma.Ui.Data.TreeModError
import Chiasma.Ui.Data.View (View, consLayout, consPane, Layout, Pane, ViewTree, Tree(..), TreeSub(..))
import Chiasma.Ui.ViewTree (togglePane)
import Ribosome.Msgpack.NvimObject (NO(..))
import qualified Ribosome.Control.Ribo as Ribo (state)
import Myo.Data.Myo (Myo)
import Myo.Data.Env (Env(ui))
import Myo.Test.Unit (tmuxSpecWithDef)
import Myo.Tmux.IO (liftTmux)
import Myo.Ui.View
import Myo.Ui.Data.ViewCoords (viewCoords)
import Myo.Ui.Data.UiState (UiState(..))
import Myo.Ui.Data.Space (Space(Space))
import Myo.Ui.Data.Window (Window(Window))
import Myo.Ui.Toggle (myoTogglePane)
import Config (vars)
import qualified Myo.Log as Log

layout :: View Layout
layout = consLayout (Ident.Str "l")

pane1 :: View Pane
pane1 = consPane (Ident.Str "p1")

pane2 :: View Pane
pane2 = consPane (Ident.Str "p2")

setupTree :: Myo ()
setupTree = do
  result <- runExceptT setup
  case result of
    Right _ -> return ()
    Left err -> throwString $ show err
  where
    setup = do
      insertLayout (viewCoords "s" "w" "wroot") layout
      insertPane (viewCoords "s" "w" "l") pane1
      insertPane (viewCoords "s" "w" "l") pane2

togglePaneSpec :: Myo ()
togglePaneSpec = do
  _ <- createSpace (Ident.Str "s")
  _ <- createWindow (viewCoords "s" "w" "wroot")
  setupTree
  myoTogglePane (NO (Ident.Str "p1"))
  myoTogglePane (NO (Ident.Str "p2"))
  panes <- liftTmux $ Tmux.read @Codec.Pane "list-panes" ["-a"]
  liftIO $ assertEqual (Right 3) (fmap length panes)

test_togglePane :: IO ()
test_togglePane =
  vars >>= tmuxSpecWithDef togglePaneSpec
