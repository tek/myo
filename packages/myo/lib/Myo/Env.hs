module Myo.Env where

import Chiasma.Data.Views (Views)
import Path (Abs, Dir, Path, parseRelDir, (</>))
import Path.IO (createDirIfMissing, createTempDir, getTempDir, removeDirRecur)
import System.Posix.User (getEffectiveUserName)

import Myo.Ui.Data.Space (Space)
import qualified Myo.Ui.Data.UiState as UiState
import Myo.Ui.Data.UiState (UiState)

withTempDir ::
  Members [Resource, Embed IO] r =>
  Path Abs Dir ->
  (Path Abs Dir -> Sem r a) ->
  Sem r a
withTempDir targetDir =
  bracket (createTempDir targetDir "temp") (tryAny_ . removeDirRecur)

myoViews ::
  Member (AtomicState UiState) r =>
  Sem r Views
myoViews =
  atomicGets UiState.views

myoSpaces ::
  Member (AtomicState UiState) r =>
  Sem r [Space]
myoSpaces =
  atomicGets UiState.spaces

bracketMyoTempDir ::
  Members [Resource, Embed IO] r =>
  (Path Abs Dir -> Sem r a) ->
  Sem r a
bracketMyoTempDir thunk = do
  name <- embed getEffectiveUserName
  tmp <- getTempDir
  sub <- embed (parseRelDir ("myo-" <> name))
  let base = tmp </> sub
  createDirIfMissing True base
  withTempDir base thunk
