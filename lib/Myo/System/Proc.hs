module Myo.System.Proc where

import Conduit (ConduitT, runConduit, sinkList, (.|))
import Control.Applicative (Alternative)
import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Attoparsec.ByteString (parseOnly)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (readFile)
import Data.Conduit.List (unfoldM)
import Data.Either.Combinators (rightToMaybe)
import Data.List.NonEmpty (NonEmpty((:|)))
import Ribosome.Data.Functor ((<$<))
import System.FilePath (FilePath, (</>))
import Text.Parser.Char (CharParsing, anyChar, noneOf, spaces)
import Text.Parser.Combinators (many, skipMany)
import Text.Parser.Token (TokenParsing, decimal, parens)

procStatPpid :: (Alternative m, CharParsing m, TokenParsing m, Monad m) => m Int
procStatPpid = do
  pp <- skipMany decimal >> spaces >> parens (many (noneOf ")")) >> spaces >> anyChar >> spaces >> decimal
  skipMany anyChar
  return $ fromIntegral pp

parseProcStatPpid :: ByteString -> Either String Int
parseProcStatPpid =
  parseOnly procStatPpid

procStatPath :: Int -> FilePath
procStatPath pid =
  "/proc" </> show pid </> "stat"

ppid :: MonadIO m => Int -> m (Maybe Int)
ppid =
  parse . err <$< liftIO . try @IOException . B.readFile . procStatPath
  where
    err = first (const "failed to read procstat")
    parse = rightToMaybe . (>>= parseProcStatPpid)

ppidsC :: MonadIO m => Int -> ConduitT () Int m ()
ppidsC =
  unfoldM unfolder
  where
    unfolder = fmap (\ a -> (a, a)) <$< ppid

-- |Query /proc/$pid/stat and extract the parent PID
-- recurse until PID 0 is hit
-- return all found parents, including the target PID itself, starting with the target
ppids :: MonadIO m => Int -> m (NonEmpty Int)
ppids pid =
  (pid :|) <$> runConduit (ppidsC pid .| sinkList)
