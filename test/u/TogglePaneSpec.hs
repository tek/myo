{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TogglePaneSpec(
  htf_thisModulesTests
) where

import qualified Chiasma.Codec.Data as Codec (Pane)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Chiasma.Monad.Tmux as Tmux (read)
import Chiasma.Ui.Data.TreeModError (TreeModError)
import Chiasma.Ui.Data.View (Layout, Pane, View, consLayoutVertical, consPane)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Ribosome.Control.Monad.Ribo (ConcNvimS, Ribo)
import Ribosome.Data.Time (sleep)
import Ribosome.Msgpack.NvimObject (NO(..))
import Test.Framework
import UnliftIO.Exception (throwString)

import Config (vars)
import Myo.Data.Myo (Env, Myo)
import Myo.Test.Unit (tmuxExternalSpec)
import Myo.Tmux.IO (runTmuxE)
import Myo.Ui.Data.ViewCoords (viewCoords)
import Myo.Ui.Toggle (myoTogglePane)
import Myo.Ui.View (createSpace, createWindow, insertLayout, insertPane)

layout :: View Layout
layout = consLayoutVertical (Ident.Str "l")

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
    setup :: ExceptT TreeModError (Ribo Env (ConcNvimS Env)) ()
    setup = do
      insertLayout (viewCoords "s" "w" "wroot") layout
      insertPane (viewCoords "s" "w" "l") pane1
      insertPane (viewCoords "s" "w" "l") pane2

togglePaneSpec :: Myo ()
togglePaneSpec = do
  _ <- createSpace (Ident.Str "s")
  _ <- createWindow (viewCoords "s" "w" "wroot")
  setupTree
  lift $ myoTogglePane (NO (Ident.Str "p1"))
  lift $ myoTogglePane (NO (Ident.Str "p2"))
  panes <- runTmuxE $ Tmux.read @Codec.Pane "list-panes" ["-a"]
  liftIO $ assertEqual (Right 3) (fmap length panes)

test_togglePane :: IO ()
test_togglePane =
  vars >>= tmuxExternalSpec togglePaneSpec
