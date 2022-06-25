module Myo.Interpreter.Proc where

import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Attoparsec.Text (parseOnly)
import Data.List (dropWhileEnd)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty ((<|))
import Data.Text.IO (readFile)
import Path (Abs, Dir, File, Path, absdir, dirname, parseRelDir, relfile, toFilePath, (</>))
import Path.IO (doesPathExist, listDir)
import Text.Parser.Char (anyChar, noneOf, spaces)
import Text.Parser.Combinators (skipMany)
import Text.Parser.Token (TokenParsing, decimal, parens)

import Myo.Command.Data.Pid (Pid (Pid))
import Myo.Data.ProcError (ProcError (ProcError))
import Myo.Effect.Proc (Proc (ChildPids, Exists, ParentPids))

procStatPpid ::
  Monad m =>
  TokenParsing m =>
  m Pid
procStatPpid = do
  pp <- skipMany decimal >> spaces >> parens (many (noneOf ")")) >> spaces >> anyChar >> spaces >> decimal
  skipMany anyChar
  pure . Pid $ fromIntegral pp

parseProcStatPpid :: Text -> Maybe Pid
parseProcStatPpid =
  rightToMaybe . parseOnly procStatPpid

procStatPath :: Pid -> Maybe (Path Abs File)
procStatPath (Pid pid) = do
  parseRelDir (show pid) <&> \ pidDir ->
    [absdir|/proc|] </> pidDir </> [relfile|stat|]

ppid ::
  Member (Embed IO) r =>
  Pid ->
  Sem r (Maybe Pid)
ppid pid =
  runMaybeT do
    path <- MaybeT (pure (procStatPath pid))
    content <- MaybeT (tryIOErrorMaybe (readFile (toFilePath path)))
    MaybeT (pure (parseProcStatPpid content))

ppids ::
  Member (Embed IO) r =>
  Pid ->
  Sem r (NonEmpty Pid)
ppids startPid =
  NonEmpty.reverse <$> spin [startPid]
  where
    spin parents@(pid :| _) =
      ppid pid >>= \case
        Just parent | elem @[] parent [pid, 0, 1] ->
          pure parents
        Just parent ->
          spin (parent <| parents)
        Nothing ->
          pure parents

dirName :: Path Abs Dir -> String
dirName =
  dropWhileEnd ('/' ==) . toFilePath . dirname

matchProcDirByParent ::
  Member (Embed IO) r =>
  Pid ->
  Path Abs Dir ->
  Sem r (Maybe Pid)
matchProcDirByParent parent (dirName -> dir) =
  runMaybeT do
    pid <- MaybeT (pure (Pid <$> readMaybe dir))
    thisParent <- MaybeT (ppid pid)
    MaybeT (pure (if thisParent == parent then Just pid else Nothing))

-- |Query /proc for all PIDs and their parent PIDs, then filter those whose PPIDs match the supplied PID
childPids ::
  Members [Stop ProcError, Embed IO] r =>
  Pid ->
  Sem r [Pid]
childPids parent =
  mapMaybeM (matchProcDirByParent parent) . fst =<< stopTryIOError ProcError (listDir [absdir|/proc|])

interpretProc ::
  Member (Embed IO) r =>
  InterpreterFor (Proc !! ProcError) r
interpretProc =
  interpretResumable \case
    ChildPids parent ->
      childPids parent
    ParentPids pid ->
      ppids pid
    Exists pid -> do
      path <- stopNote (ProcError "path failure") (procStatPath pid)
      stopTryIOError (ProcError . show) (doesPathExist path)
