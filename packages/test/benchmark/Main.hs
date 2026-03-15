module Main where

import qualified Data.ByteString as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef)
import GHC.Stats (GCDetails (..), RTSStats (..), getRTSStats)
import System.Exit (exitFailure)
import qualified System.Mem as Mem

import qualified Myo.Command.Effect.CommandLog as CommandLog
import Myo.Command.Effect.CommandLog (CommandLog)
import Myo.Command.Interpreter.CommandLog (interpretCommandLog)
import Myo.Data.CommandId (CommandId)

ident :: CommandId
ident =
  "bench-cmd"

-- | A chunk of simulated terminal output, similar in size to tmux datagrams.
chunk :: ByteString
chunk =
  ByteString.replicate 200 0x61 <> "\n"

-- | Simulate a single command execution: append @n@ chunks and then archive.
simulateRun ::
  Member CommandLog r =>
  Int ->
  Sem r ()
simulateRun n = do
  replicateM_ n (CommandLog.append ident Nothing chunk)
  CommandLog.archive ident

-- | Run many append/archive cycles, forcing GC between each.
-- Returns max residency observed via 'getRTSStats'.
simulateMany ::
  Members [CommandLog, Embed IO] r =>
  Int ->
  Int ->
  Sem r Word64
simulateMany runs chunksPerRun = do
  ref <- embed (newIORef (0 :: Word64))
  replicateM_ runs do
    simulateRun chunksPerRun
    embed Mem.performGC
    stats <- embed getRTSStats
    let live = gcdetails_live_bytes (gc stats)
    embed (modifyIORef' ref (max live))
  embed (readIORef ref)

-- | Maximum acceptable residency in bytes.
-- Each run produces ~100KB of text (500 chunks * 201 bytes), so with
-- proper cleanup only the most recent prev (~100KB) should be retained,
-- plus interpreter overhead.
-- We allow 20MB as a generous bound; without the fix, residency would
-- grow to hundreds of MB over 200 runs.
maxResidencyBytes :: Word64
maxResidencyBytes =
  20_000_000

main :: IO ()
main = do
  maxLive <- runM $
    interpretCommandLog (pure 100_000) $
    simulateMany 200 500
  putStrLn ("Max residency: " <> show maxLive <> " bytes")
  when (maxLive > maxResidencyBytes) do
    putStrLn ("FAIL: max residency " <> show maxLive <> " exceeds threshold " <> show maxResidencyBytes)
    exitFailure
  putStrLn "PASS: residency stayed within bounds"
