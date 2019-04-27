{-# LANGUAGE TemplateHaskell #-}

module Myo.Init where

import Chiasma.Data.Ident (generateIdent)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Default (Default(def))
import Neovim (Neovim)
import Neovim.Context.Internal (Config(customConfig), asks')
import Path (Abs, Dir, Path)
import Ribosome.Config.Setting (setting)
import Ribosome.Control.Monad.Ribo (riboE2ribo, runRib)
import Ribosome.Control.Ribosome (Ribosome, newRibosome)
import Ribosome.Error.Report (reportError')
import Ribosome.Internal.IO (retypeNeovim)
import Ribosome.Orphans ()
import System.Log.Logger (Priority(ERROR), setLevel, updateGlobalLogger)

import Myo.Command.Data.Command (CommandLanguage(CommandLanguage))
import Myo.Command.Parse (addHandler)
import Myo.Data.Env (Env(_instanceIdent, _tempDir), Myo, MyoN)
import Myo.Orphans ()
import Myo.Output.Data.OutputHandler (OutputHandler(OutputHandler))
import Myo.Output.Lang.Haskell.Parser (haskellOutputParser)
import Myo.Output.Lang.Scala.Parser (scalaOutputParser)
import qualified Myo.Settings as Settings (detectUi)
import Myo.Tmux.Runner (addTmuxRunner)
import Myo.Ui.Default (detectDefaultUi)

initialize'' :: MyoN ()
initialize'' = do
  addTmuxRunner
  addHandler (CommandLanguage "haskell") (OutputHandler haskellOutputParser)
  addHandler (CommandLanguage "scala") (OutputHandler scalaOutputParser)
  detect <- setting Settings.detectUi
  when detect detectDefaultUi

initialize' :: Myo (Ribosome Env)
initialize' = do
  result <- riboE2ribo initialize''
  reportError' "init" result
  lift $ asks' customConfig

initialize :: Path Abs Dir -> Neovim e (Ribosome Env)
initialize tmpdir = do
  liftIO $ updateGlobalLogger "Neovim.Plugin" (setLevel ERROR)
  iid <- generateIdent
  ribo <- newRibosome "myo" def { _instanceIdent = iid, _tempDir = tmpdir }
  retypeNeovim (const ribo) (runRib initialize')
