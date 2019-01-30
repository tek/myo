{-# OPTIONS_GHC -F -pgmF htfpp #-}

module ToggleLayoutSpec(
  htf_thisModulesTests
) where

import Test.Framework
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import UnliftIO.Exception (throwString)
import qualified Chiasma.Monad.Tmux as Tmux (read)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import qualified Chiasma.Codec.Data as Codec (Pane)
import Chiasma.Ui.Data.View (View, consLayout, consPane, Layout, Pane)
import Ribosome.Msgpack.NvimObject (NO(..))
import Myo.Data.Myo (Myo)
import Myo.Test.Unit (tmuxSpecWithDef)
import Myo.Tmux.IO (liftTmux)
import Myo.Ui.View
import Myo.Ui.Data.ViewCoords (viewCoords)
import Myo.Ui.Toggle (myoToggleLayout)
import Config (vars)

layout :: View Layout
layout = consLayout (Ident.Str "l")

pane1 :: View Pane
pane1 = consPane (Ident.Str "p1")

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

toggleLayoutSpec :: Myo ()
toggleLayoutSpec = do
  _ <- createSpace (Ident.Str "s")
  _ <- createWindow (viewCoords "s" "w" "wroot")
  setupTree
  myoToggleLayout (NO (Ident.Str "l"))
  panes <- liftTmux $ Tmux.read @Codec.Pane "list-panes" ["-a"]
  liftIO $ assertEqual (Right 2) (fmap length panes)

test_toggleLayout :: IO ()
test_toggleLayout =
  vars >>= tmuxSpecWithDef toggleLayoutSpec
