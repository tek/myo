module Myo.Command.Data.CommandOutput where

import Data.ByteString.Builder (Builder)
import qualified Data.Text as Text

data OutputChunks =
  OutputChunks {
    chunks :: Seq ByteString,
    size :: Int
  }
  deriving stock (Eq, Show)

instance Default OutputChunks where
  def =
    OutputChunks mempty 0

outputChunksEmpty :: OutputChunks -> Bool
outputChunksEmpty OutputChunks {size} =
  size == 0

data CurrentOutput =
  Unbuilt OutputChunks
  |
  PartiallyBuilt OutputChunks Text
  |
  Built Text
  deriving stock (Eq, Show)

instance Default CurrentOutput where
  def = Unbuilt def

currentEmpty :: CurrentOutput -> Bool
currentEmpty = \case
  Unbuilt c -> outputChunksEmpty c
  PartiallyBuilt s t -> outputChunksEmpty s && Text.null t
  Built t -> Text.null t

data CommandOutput =
  CommandOutput {
    prev :: Maybe (Either Text Builder),
    current :: CurrentOutput
  }
  deriving stock (Generic)

instance Default CommandOutput where
  def = CommandOutput Nothing def
