module Myo.Command.Data.PendingCommand where

-- import Chiasma.Data.Ident (Identifiable(..))
-- import Data.Hourglass (Elapsed)
-- import qualified Text.Show

-- import Process (Pid)

-- data PendingCommand =
--   PendingCommand {
--     mecpCommand :: Ident,
--     mecpFindPid :: IO (Maybe Pid),
--     mecpStartTime :: Elapsed
--   }

-- instance Text.Show.Show PendingCommand where
--   show (PendingCommand cmd _ st) =
--     "PendingCommand" <> show (cmd, st)

-- instance Identifiable PendingCommand where
--   identify = mecpCommand
