module Myo.Temp where

import Exon (exon)
import Path (Abs, Dir, Path, parseRelDir, (</>))
import Path.IO (createDirIfMissing, createTempDir, getTempDir, removeDirRecur)
import Ribosome (BootError (BootError))
import System.IO.Error (IOError)
import System.Posix.User (getEffectiveUserName)

import Myo.Command.Data.LogDir (LogDir (LogDir))

withTempDir ::
  Members [Error BootError, Resource, Embed IO] r =>
  Path Abs Dir ->
  (Path Abs Dir -> Sem r a) ->
  Sem r a
withTempDir targetDir =
  bracket (fromExceptionVia ioError (createTempDir targetDir "temp")) (tryAny_ . removeDirRecur)
  where
    ioError (e :: IOError) =
      BootError [exon|Couldn't create temp dir: #{show e}|]

withLogDir ::
  Members [Error BootError, Resource, Embed IO] r =>
  (Path Abs Dir -> Sem r a) ->
  Sem r a
withLogDir thunk = do
  name <- embed getEffectiveUserName
  tmp <- getTempDir
  sub <- note "User name can't be used in path" (parseRelDir ("myo-" <> name))
  let base = tmp </> sub
  createDirIfMissing True base
  withTempDir base thunk

interpretLogDir ::
  Members [Error BootError, Resource, Embed IO] r =>
  InterpreterFor (Reader LogDir) r
interpretLogDir sem =
  withLogDir \ d ->
    runReader (LogDir d) sem
