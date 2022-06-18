module Myo.Ui.UpsertView where

-- import Chiasma.Data.Ident (sameIdent)
-- import Chiasma.Ui.Data.View (
--   LayoutView,
--   PaneView,
--   Tree(..),
--   TreeSub(..),
--   View(View),
--   ViewTree,
--   )
-- import qualified Control.Lens as Lens (each, over, transform)

-- import Myo.Ui.Data.Space (Space)
-- import qualified Myo.Ui.Data.Space as Space (windows)
-- import qualified Myo.Ui.Data.Window as Window (layout)

-- upsertLayout :: Ident -> LayoutView -> ViewTree -> ViewTree
-- upsertLayout _ newLayout@(View newIdent _ _ _) (Tree (View currentIdent _ _ _) sub) | matching =
--   Tree newLayout sub
--   where
--     matching = newIdent == currentIdent
-- upsertLayout layoutIdent newLayout@(View newIdent _ _ _) (Tree layout@(View currentIdent _ _ _) sub) | matching =
--   Tree layout (TreeNode (Tree newLayout []) : sub)
--   where
--     matching = layoutIdent == currentIdent && not (any sameLayoutIdent sub)
--     sameLayoutIdent (TreeNode (Tree (View ident _ _ _) _)) =
--       ident == newIdent
--     sameLayoutIdent _ =
--       False
-- upsertLayout _ _ tree =
--   tree

-- upsertPane :: Ident -> PaneView -> ViewTree -> ViewTree
-- upsertPane layoutIdent newPane@(View newIdent _ _ _) (Tree layout@(View currentIdent _ _ _) sub) | matching =
--   Tree layout newSub
--   where
--     matching =
--       layoutIdent == currentIdent && any samePaneIdent sub
--     newSub =
--       replace <$> sub
--     replace (TreeLeaf (View ident _ _ _)) | newIdent == ident =
--       TreeLeaf newPane
--     replace a =
--       a
--     samePaneIdent (TreeLeaf pane) =
--       sameIdent pane newPane
--     samePaneIdent _ =
--       False
-- upsertPane layoutIdent newPane (Tree layout@(View currentIdent _ _ _) sub) | layoutIdent == currentIdent =
--   Tree layout (TreeLeaf newPane : sub)
-- upsertPane _ _ tree =
--   tree

-- upsertLayoutSpaces :: Ident -> LayoutView -> [Space] -> [Space]
-- upsertLayoutSpaces ident view =
--   Lens.over (Lens.each . Space.windows . Lens.each . Window.layout) (Lens.transform $ upsertLayout ident view)

-- upsertPaneSpaces :: Ident -> PaneView -> [Space] -> [Space]
-- upsertPaneSpaces ident view =
--   Lens.over (Lens.each . Space.windows . Lens.each . Window.layout) (Lens.transform $ upsertPane ident view)
