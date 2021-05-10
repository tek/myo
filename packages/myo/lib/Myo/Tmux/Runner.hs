module Myo.Tmux.Runner where

import Chiasma.Data.Ident (Ident(Str))
import Chiasma.Data.Views (Views, ViewsError)
import Ribosome.Tmux.Run (RunTmux)

import Myo.Command.Data.RunError (RunError)
import Myo.Command.Runner (RunInIO, addRunner, extractRunError)
import Myo.Data.Env (Env)
import Myo.Tmux.Run (tmuxCanRun, tmuxCapture, tmuxCheckPending, tmuxRun)

addTmuxRunner ::
  MonadRibo m =>
  NvimE e m =>
  RunTmux m =>
  MonadBaseControl IO m =>
  MonadDeepState s Env m =>
  MonadDeepState s Views m =>
  MonadDeepError e RunError m =>
  MonadDeepError e ViewsError m =>
  RunInIO m =>
  m ()
addTmuxRunner =
  void $ addRunner (Str "tmux") (extractRunError tmuxRun) (extractRunError tmuxCheckPending) tmuxCanRun (Just capture)
  where
    capture =
      fmap Right . tmuxCapture
