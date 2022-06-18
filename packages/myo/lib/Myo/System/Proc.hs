module Myo.System.Proc where

-- import Conduit (ConduitT, runConduit, sinkList, (.|))
-- import Data.Attoparsec.Text (parseOnly)
-- import Data.Conduit.List (unfoldM)
-- import Path (Abs, Dir, absdir, toFilePath)
-- import Path.IO (listDir)
-- import Relude.Extra (dup)
-- import Ribosome.Control.Exception (catchAnyAs)
-- import System.FilePath ((</>))
-- import Text.Parser.Char (CharParsing, anyChar, noneOf, spaces)
-- import Text.Parser.Combinators (skipMany)
-- import Text.Parser.Token (TokenParsing, decimal, parens)
-- import Text.RE.PCRE.Text (RE, matchedText, re, (?=~))
-- import UnliftIO.Directory (doesPathExist)

-- import Myo.Command.Data.Pid (Pid (Pid))

-- procStatPpid :: (Alternative m, CharParsing m, TokenParsing m, Monad m) => m Pid
-- procStatPpid = do
--   pp <- skipMany decimal >> spaces >> parens (many (noneOf ")")) >> spaces >> anyChar >> spaces >> decimal
--   skipMany anyChar
--   pure . Pid $ fromIntegral pp

-- parseProcStatPpid :: Text -> Either Text Pid
-- parseProcStatPpid =
--   mapLeft toText . parseOnly procStatPpid

-- procStatPath :: Pid -> FilePath
-- procStatPath (Pid pid) =
--   "/proc" </> show pid </> "stat"

-- ppid ::
--   MonadIO m =>
--   Pid ->
--   m (Maybe Pid)
-- ppid pid =
--   catchAnyAs Nothing do
--     !output <- readFile (procStatPath pid)
--     pure (rightToMaybe . parseProcStatPpid . toText $ output)

-- ppidsC ::
--   MonadIO m =>
--   Pid ->
--   ConduitT () Pid m ()
-- ppidsC =
--   unfoldM unfolder
--   where
--     unfolder = fmap (fmap dup) <$> ppid

-- -- |Query /proc/$pid/stat and extract the parent PID
-- -- recurse until PID 0 is hit
-- -- pure all found parents, including the target PID itself, starting with the target
-- ppids ::
--   MonadIO m =>
--   Pid ->
--   m (NonEmpty Pid)
-- ppids pid =
--   (pid :|) <$> runConduit (ppidsC pid .| sinkList)

-- pidRegex :: RE
-- pidRegex =
--   [re|[0-9]+|]

-- -- |Query /proc for all PIDs and their parent PIDs, then filter those whose PPIDs match the supplied PID
-- childPids ::
--   MonadIO m =>
--   Pid ->
--   m [Pid]
-- childPids pid = do
--   (dirs, _) <- listDir [absdir|/proc|]
--   let
--     pidStrings = catMaybes ((\ a -> matchedText (a ?=~ pidRegex)) . toText . toFilePath <$> dirs)
--     pids = mapMaybe (rightToMaybe . fmap Pid . readEither . toString) pidStrings
--   mapMaybe matchParent <$> traverse withPpid pids
--   where
--     withPpid pid' =
--       fmap (pid',) <$> ppid pid'
--     matchParent (Just (p, pp)) | pp == pid =
--       Just p
--     matchParent _ =
--       Nothing

-- processExists ::
--   MonadIO m =>
--   Pid ->
--   m Bool
-- processExists =
--   catchAnyAs False . doesPathExist . procStatPath
