module Monolog.Text
  ( parseText,
    printText,
  )
where

import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Data.Time
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Monolog.Types

printText :: [Note] -> Lazy.Text
printText = Builder.toLazyText . mconcat . List.intersperse (Builder.fromText separator <> "\n") . fmap build
  where
    build :: Note -> Builder.Builder
    build n =
      mconcat
        [ Builder.fromText prefixUUID,
          Builder.fromText $ UUID.toText n.id,
          "\n",
          Builder.fromText prefixCreated,
          Builder.fromString $ fmtTime n.created,
          "\n",
          maybe mempty (\t -> Builder.fromText prefixArchived <> Builder.fromString (fmtTime t) <> "\n") n.archived,
          "\n",
          Builder.fromText n.body.toText,
          "\n"
        ]

parseText :: Lazy.Text -> [PartialNote]
parseText = parseNote . fmap Lazy.toStrict . Lazy.lines
  where
    parseNote :: [Text] -> [PartialNote]
    parseNote [] = []
    parseNote (line : ls) | line == separator = parseNote ls
    parseNote ls = parseUUID ls
    parseUUID :: [Text] -> [PartialNote]
    parseUUID (line : ls)
      | Just muuid <- Text.stripPrefix prefixUUID line,
        Just uuid <- UUID.fromText muuid =
          parseCreated (Just uuid) ls
    parseUUID ls = parseCreated Nothing ls
    parseCreated :: Maybe UUID -> [Text] -> [PartialNote]
    parseCreated muuid (line : ls)
      | Just mtime <- Text.stripPrefix prefixCreated line,
        Just time <- parseTime (Text.unpack mtime) =
          parseArchived muuid (Just time) ls
    parseCreated muuuid ls = parseArchived muuuid Nothing ls
    parseArchived :: Maybe UUID -> Maybe ZonedTime -> [Text] -> [PartialNote]
    parseArchived muuid mcreated (line : ls)
      | Just mtime <- Text.stripPrefix prefixArchived line,
        Just time <- parseTime (Text.unpack mtime) =
          parseBody muuid mcreated (Just time) [] ls
    parseArchived muuid mcreated ls = parseBody muuid mcreated Nothing [] ls
    parseBody muuid mcreated marchived body (line : ls)
      | line /= separator = parseBody muuid mcreated marchived (line : body) ls
    parseBody muuid mcreated marchived body ls = PartialNote muuid mcreated marchived (Text.unlines . reverse $ body) : parseNote ls

fmt :: String
fmt = "%Y-%m-%d %H:%M:%S %Ez"

parseTime :: String -> Maybe ZonedTime
parseTime = parseTimeM False defaultTimeLocale fmt

fmtTime :: ZonedTime -> String
fmtTime = formatTime defaultTimeLocale fmt

separator :: Text
separator = "~~~~~~~~~"

prefixUUID, prefixCreated, prefixArchived :: Text
prefixUUID = "uuid:     "
prefixCreated = "created:  "
prefixArchived = "archived: "
