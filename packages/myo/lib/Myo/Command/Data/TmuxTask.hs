module Myo.Command.Data.TmuxTask where

import Chiasma.Data.TmuxId (PaneId)

import qualified Myo.Command.Data.Command
import Myo.Command.Data.Command (Command)
import Myo.Command.Data.CommandSpec (renderCommandSpec)
import Myo.Command.Data.UiTarget (UiTarget)

data TaskType =
  Shell
  |
  Wait
  |
  Kill
  deriving stock (Eq, Show)

data TmuxTask =
  TmuxTask {
    taskType :: TaskType,
    target :: UiTarget,
    pane :: PaneId,
    command :: Command,
    compiled :: [Text]
  }
  deriving stock (Eq, Show)

noParams ::
  TaskType ->
  UiTarget ->
  PaneId ->
  Command ->
  TmuxTask
noParams taskType target pane command =
  TmuxTask {compiled = renderCommandSpec command.cmdLines, ..}
