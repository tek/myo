module Myo.Command.CommandSpec.Render where

import qualified Data.Text as Text
import Exon (exon)

import Myo.Command.Data.CommandTemplate (CommandSegment (..), ParamSegment (..))
import Myo.Command.Data.Param (paramTagName)

renderSub :: ParamSegment x -> Text
renderSub = \case
  ParamRequired -> ""
  ParamOptional -> ":"
  ParamTemplate tpl -> [exon|:#{foldMap renderSegment tpl}|]
  ParamFlagTemplate tpl -> [exon|?#{foldMap renderSegment tpl}|]

renderSegment :: CommandSegment -> Text
renderSegment = \case
  SegmentLit s -> Text.replace "{" "\\{" (Text.replace "}" "\\}" s)
  SegmentParam pid opt -> [exon|{#{paramTagName pid}#{renderSub opt}}|]
