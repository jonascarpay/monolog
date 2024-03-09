module Monolog.Note
  ( Note (..),
    mkNote,
    mkBody,
    PartialNote (..),
    Body (toText),
    Timestamp (..),
    fromPartial,
  )
where

import Data.Aeson
import Data.Char
import Data.List qualified as List
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import Monolog.DateTime
import Monolog.Parser (parseTimeStamps)

newtype Body = MkBody {toText :: Text}
  deriving newtype (Show, ToJSON)

data Note = UnsafeMkNote
  { id :: UUID,
    created :: ZonedTime,
    archived :: Maybe ZonedTime,
    body :: Body,
    timestamps :: [Timestamp]
  }
  deriving stock (Generic, Show)

data PartialNote = PartialNote
  { id :: Maybe UUID,
    created :: Maybe ZonedTime,
    archive :: Maybe ZonedTime,
    body :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

mkNote :: UUID -> ZonedTime -> Maybe ZonedTime -> Text -> Note
mkNote uuid created archived bodyRaw = UnsafeMkNote uuid created archived body tss
  where
    body = mkBody bodyRaw
    tss = mapMaybe (fromPartialTimestamp created) $ parseTimeStamps body.toText

fromPartial :: PartialNote -> IO Note
fromPartial (PartialNote pid popen archive_time body) = do
  uuid <- maybe UUID.nextRandom pure pid
  open_time <- maybe getZonedTime pure popen
  pure $ mkNote uuid open_time archive_time body

mkBody :: Text -> Body
mkBody =
  MkBody
    . Text.unlines
    . List.dropWhileEnd Text.null
    . dropWhile Text.null
    . fmap (escapeSeparator . Text.dropWhileEnd isSpace . dropWindowsLineEndings)
    . Text.lines
  where
    dropWindowsLineEndings t = case Text.unsnoc t of
      Just (t', '\r') -> t'
      _ -> t
    escapeSeparator "~~~~~~~~~" = " ~~~~~~~~~"
    escapeSeparator line = line
