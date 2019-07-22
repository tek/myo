{-# LANGUAGE QuasiQuotes #-}

module Myo.System.Proc where

import Conduit (ConduitT, runConduit, sinkList, (.|))
import Control.Applicative (Alternative)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Attoparsec.Text (parseOnly)
import Data.Conduit.List (unfoldM)
import Data.List.NonEmpty (NonEmpty((:|)))
import Path (absdir, toFilePath)
import Path.IO (listDir)
import Ribosome.Control.Exception (catchAnyAs, tryAny)
import System.FilePath (FilePath, (</>))
import Text.Parser.Char (CharParsing, anyChar, noneOf, spaces)
import Text.Parser.Combinators (many, skipMany)
import Text.Parser.Token (TokenParsing, decimal, parens)
import Text.RE.PCRE.Text (RE, matchedText, re, (?=~))
import UnliftIO.Directory (doesPathExist)

import Myo.Command.Data.Pid (Pid(Pid))

procStatPpid :: (Alternative m, CharParsing m, TokenParsing m, Monad m) => m Pid
procStatPpid = do
  pp <- skipMany decimal >> spaces >> parens (many (noneOf ")")) >> spaces >> anyChar >> spaces >> decimal
  skipMany anyChar
  return . Pid $ fromIntegral pp

parseProcStatPpid :: Text -> Either Text Pid
parseProcStatPpid =
  mapLeft toText . parseOnly procStatPpid

procStatPath :: Pid -> FilePath
procStatPath (Pid pid) =
  "/proc" </> show pid </> "stat"

ppid ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Pid ->
  m (Maybe Pid)
ppid =
  parse . bimap err toText <$$> tryAny . readFile . procStatPath
  where
    err =
      const "failed to read procstat"
    parse =
      rightToMaybe . (>>= parseProcStatPpid)

ppidsC ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Pid ->
  ConduitT () Pid m ()
ppidsC =
  unfoldM unfolder
  where
    unfolder = fmap (\ a -> (a, a)) <$$> ppid

-- |Query /proc/$pid/stat and extract the parent PID
-- recurse until PID 0 is hit
-- return all found parents, including the target PID itself, starting with the target
ppids ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Pid ->
  m (NonEmpty Pid)
ppids pid =
  (pid :|) <$> runConduit (ppidsC pid .| sinkList)

pidRegex :: RE
pidRegex =
  [re|[0-9]+|]

-- |Query /proc for all PIDs and their parent PIDs, then filter those whose PPIDs match the supplied PID
childPids ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Pid ->
  m [Pid]
childPids pid = do
  (dirs, _) <- listDir [absdir|/proc|]
  let
    pidStrings = catMaybes ((\ a -> matchedText (a ?=~ pidRegex)) . toText . toFilePath <$> dirs)
    pids = mapMaybe (rightToMaybe . fmap Pid . readEither) pidStrings
  mapMaybe matchParent <$> traverse withPpid pids
  where
    withPpid pid' =
      (pid',) <$$> ppid pid'
    matchParent (Just (p, pp)) | pp == pid =
      Just p
    matchParent _ =
      Nothing

processExists ::
  MonadIO m =>
  MonadBaseControl IO m =>
  Pid ->
  m Bool
processExists =
  catchAnyAs False . doesPathExist . procStatPath
