module Myo.Command.Data.CommandTemplate where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Attoparsec.Text (Parser, char, choice, parseOnly, satisfy, try)
import Data.Char (isAlphaNum)
import Data.GADT.Compare (GEq, defaultEq)
import Data.GADT.Compare.TH (DeriveGEQ (deriveGEq))
import Data.GADT.Show (GShow (gshowsPrec))
import Data.GADT.Show.TH (deriveGShow)
import Exon (exon)
import Prelude hiding (try)
import Ribosome (MsgpackDecode (fromMsgpack), MsgpackEncode (toMsgpack))
import Ribosome.Msgpack (decodeError)

import Myo.Command.Data.Param (ParamTag (ParamBool, ParamText))

data ParamSegment a where
  ParamRequired :: ParamSegment Text
  ParamOptional :: ParamSegment Text
  ParamTemplate :: NonEmpty CommandSegment -> ParamSegment Text
  ParamFlagTemplate :: NonEmpty CommandSegment -> ParamSegment Bool

data CommandSegment =
  SegmentLit Text
  |
  ∀ a . SegmentParam (ParamTag a) (ParamSegment a)

instance GEq ParamSegment => Eq CommandSegment where
  SegmentLit l == SegmentLit r = l == r
  SegmentParam (ParamText ltag) lsub == SegmentParam (ParamText rtag) rsub = ltag == rtag && defaultEq lsub rsub
  SegmentParam (ParamBool ltag) lsub == SegmentParam (ParamBool rtag) rsub = ltag == rtag && defaultEq lsub rsub
  _ == _ = False

instance GShow ParamSegment => Show CommandSegment where
  showsPrec d = \case
    SegmentLit t -> showParen (d > 10) [exon|SegmentLit #{showsPrec 11 t}|]
    SegmentParam ptag sub -> showParen (d > 10) [exon|SegmentParam #{gshowsPrec 11 ptag} #{gshowsPrec 11 sub}|]

data CommandTemplate =
  CommandTemplate {
    rendered :: [Text],
    segments :: [NonEmpty CommandSegment]
  }

deriving stock instance Eq CommandSegment => Eq CommandTemplate
deriving stock instance Show CommandSegment => Show CommandTemplate

deriveGEq ''ParamSegment
deriveGShow ''ParamSegment

isKeyword :: Char -> Bool
isKeyword c =
  isAlphaNum c || c == '_'

isBrace :: Char -> Bool
isBrace c =
  c == '{' || c == '}'

paramSub :: Text -> Maybe (Bool, [CommandSegment]) -> CommandSegment
paramSub pid = \case
  Nothing -> SegmentParam (ParamText pid) ParamRequired
  Just (_, []) -> SegmentParam (ParamText pid) ParamOptional
  Just (False, h : t) -> SegmentParam (ParamText pid) (ParamTemplate (h :| t))
  Just (True, h : t) -> SegmentParam (ParamBool pid) (ParamFlagTemplate (h :| t))

segmentParam :: Parser CommandSegment
segmentParam = do
  _ <- char '{'
  pid <- many (satisfy isKeyword)
  sub <- optional (pairA (choice [False <$ char ':', True <$ char '?']) segments)
  char '}'
  pure (paramSub (fromString pid) sub)

segmentLit :: Parser CommandSegment
segmentLit =
  SegmentLit . toText <$> some (choice [escapedBrace, satisfy (not . isBrace)])
  where
    escapedBrace = try (char '\\' *> satisfy isBrace)

segments :: Parser [CommandSegment]
segments = many (choice [segmentParam, segmentLit])

parseCommandSegments :: Text -> Either Text (NonEmpty CommandSegment)
parseCommandSegments raw = do
  segs <- first toText (parseOnly segments raw)
  maybeToRight "Empty command line" (nonEmpty segs)

parseCommandTemplate :: Either Text [Text] -> Either Text CommandTemplate
parseCommandTemplate raw =
  CommandTemplate lns <$> traverse parseCommandSegments lns
  where
    lns = either pure id raw

parseCommandTemplate' :: [Text] -> CommandTemplate
parseCommandTemplate' raw =
  fromRight (CommandTemplate raw (pure . SegmentLit <$> raw)) (parseCommandTemplate (Right raw))

instance MsgpackDecode CommandTemplate where
  fromMsgpack = leftA decodeError . parseCommandTemplate <=< fromMsgpack

instance FromJSON CommandTemplate where
  parseJSON v = do
    lns <- (Right <$> parseJSON v) <|> (Left <$> parseJSON v)
    leftA (fail . toString) (parseCommandTemplate lns)

renderTemplate :: CommandTemplate -> [Text]
renderTemplate (CommandTemplate rendered _) = rendered

instance MsgpackEncode CommandTemplate where
  toMsgpack = toMsgpack . renderTemplate

instance ToJSON CommandTemplate where
  toJSON = toJSON . renderTemplate
