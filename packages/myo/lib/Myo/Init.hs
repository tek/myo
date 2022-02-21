module Myo.Init where

import Chiasma.Data.Ident (generateIdent)
import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Path (Abs, Dir, Path)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Monad.Ribo (RNeovim, runRibo)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError')
import Ribosome.Internal.IO (retypeNeovim)
import Ribosome.Orphans ()
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

import Myo.Command.Data.Command (CommandLanguage(CommandLanguage))
import Myo.Command.History (loadHistory)
import Myo.Command.Parse (addHandler)
import Myo.Command.Subproc.Runner (addSubprocessRunner)
import Myo.Data.Env (Env(_instanceIdent, _tempDir), Myo)
import Myo.Orphans ()
import Myo.Output.Data.OutputHandler (OutputHandler(OutputHandler))
import Myo.Output.Lang.Haskell.Parser (haskellOutputParser)
import Myo.Output.Lang.Nix.Parser (nixOutputParser)
import Myo.Output.Lang.Scala.Parser (scalaOutputParser)
import qualified Myo.Settings as Settings (detectUi)
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Default (detectDefaultUi)
import Myo.Vim.Runner (addVimRunner)

initialize'' :: Myo ()
initialize'' = do
  addSubprocessRunner
  addTmuxRunner
  addVimRunner
  addHandler (CommandLanguage "nix") (OutputHandler nixOutputParser)
  addHandler (CommandLanguage "haskell") (OutputHandler haskellOutputParser)
  addHandler (CommandLanguage "scala") (OutputHandler scalaOutputParser)
  detect <- setting Settings.detectUi
  when detect detectDefaultUi

initialize' :: RNeovim Env (Ribosome Env)
initialize' = do
  result <- runRibo initialize''
  reportError' "init" result
  asks' customConfig

initialize :: Path Abs Dir -> Neovim e (Ribosome Env)
initialize tmpdir = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  iid <- generateIdent
  ribo <- newRibosome "myo" def { _instanceIdent = iid, _tempDir = tmpdir }
  retypeNeovim (const ribo) initialize'

-- |depends on proteome
myoStage4 ::
  Myo ()
myoStage4 =
  loadHistory
