module IO (readNoteDb, writeNoteDb) where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Data.Text.Lazy.IO qualified as Lazy
import Data.Time
import Data.UUID (UUID, fromText, toText)
import NoteDb

fmt :: String
fmt = "%Y-%m-%d %H:%M:%S %Ez"

fmtTime :: ZonedTime -> String
fmtTime = formatTime defaultTimeLocale fmt

parseTime :: String -> Maybe ZonedTime
parseTime = parseTimeM False defaultTimeLocale fmt

separator :: Text
separator = "~~~~~~~~~"

printText :: [Note] -> Lazy.Text
printText = Builder.toLazyText . mconcat . intersperse (Builder.fromText separator <> "\n") . fmap build
  where
    build n =
      mconcat
        [ Builder.fromText prefixUUID,
          Builder.fromText $ toText n.id,
          "\n",
          Builder.fromText prefixCreated,
          Builder.fromString $ fmtTime n.open_time,
          "\n",
          maybe mempty (\t -> Builder.fromText prefixArchived <> Builder.fromString (fmtTime t) <> "\n") n.archive_time,
          "\n",
          Builder.fromText $ strip n.body,
          "\n"
        ]

prefixUUID, prefixCreated, prefixArchived :: Text
prefixUUID = "uuid:     "
prefixCreated = "created:  "
prefixArchived = "archived: "

readNoteDb :: FilePath -> IO NoteDb
readNoteDb path = do
  txt <- Lazy.readFile path
  let pnotes = parseText txt
  notes <- traverse fromPartial pnotes
  pure $ fromList notes

writeNoteDb :: FilePath -> NoteDb -> IO ()
writeNoteDb fp = Lazy.writeFile fp . printText . toAscList

strip :: Text -> Text
strip = Text.dropWhileEnd (\c -> c == ' ' || c == '\n') . Text.dropWhile (== '\n')

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
        Just uuid <- fromText muuid =
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
    parseBody muuid mcreated marchived body ls = PartialNote muuid mcreated marchived (strip . Text.unlines . reverse $ body) : parseNote ls
