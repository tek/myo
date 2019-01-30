module Myo.Test.Unit(
  spec,
  specWith,
  specWithDef,
  specConfig,
  tmuxSpecWithDef,
) where

import qualified Control.Lens as Lens (over)
import Data.Default.Class (def)
import UnliftIO (throwString)
import UnliftIO.STM (newTVarIO)
import Ribosome.Config.Setting (updateSetting)
import qualified Ribosome.Control.Ribo as Ribo (modify)
import Ribosome.Test.Embed (Vars, TestConfig)
import Ribosome.Test.Unit (unitSpec)
import qualified Chiasma.Data.Ident as Ident (Ident(Str))
import Chiasma.Data.TmuxId (SessionId(..), WindowId(..), PaneId(..))
import qualified Chiasma.Data.View as Tmux (View(View))
import Chiasma.Data.Views (Views(Views))
import Chiasma.Native.Api (TmuxNative(TmuxNative))
import Chiasma.Test.Tmux (tmuxSpec)
import Chiasma.Ui.Data.View
import Myo.Data.Myo (Myo)
import Myo.Data.Env (Env)
import Myo.Settings (tmuxSocket)
import Myo.Test.Config (defaultTestConfig, defaultTestConfigWith)
import Myo.Ui.Data.Space (Space(Space))
import Myo.Ui.Data.Window (Window(Window))
import Myo.Ui.View (envViewsLens, insertSpace)

specConfig :: TestConfig -> Env -> Myo () -> IO ()
specConfig conf e s = do
  t <- newTVarIO e
  unitSpec conf t s

spec :: Env -> Myo () -> IO ()
spec =
  specConfig defaultTestConfig

specWith :: Env -> Myo () -> Vars -> IO ()
specWith e s vars = do
  t <- newTVarIO e
  unitSpec (defaultTestConfigWith vars) t s

specWithDef :: Myo () -> Vars -> IO ()
specWithDef =
  specWith def

insertInitialViews :: Views -> Views
insertInitialViews (Views sessions windows panes viewsLog) =
  Views
    (Tmux.View (Ident.Str "vim") (Just (SessionId 0)) : sessions)
    (Tmux.View (Ident.Str "vim") (Just (WindowId 0)) : windows)
    (Tmux.View (Ident.Str "vim") (Just (PaneId 0)) : panes)
    viewsLog

vimTree :: ViewTree
vimTree =
  Tree (consLayout (Ident.Str "vim")) [TreeLeaf ((consPane (Ident.Str "vim")) { extra = Pane True False Nothing })]

withTmux :: Myo () -> TmuxNative -> Myo ()
withTmux thunk (TmuxNative (Just socket)) = do
  updateSetting tmuxSocket socket
  _ <- insertSpace $ Space (Ident.Str "vim") [Window (Ident.Str "vim") vimTree]
  Ribo.modify $ Lens.over envViewsLens insertInitialViews
  thunk
withTmux _ _ = throwString "no socket in test tmux"

tmuxSpecWithDef :: Myo () -> Vars -> IO ()
tmuxSpecWithDef thunk vars =
  tmuxSpec $ \api -> specWithDef (withTmux thunk api) vars
