module NoteDb
  ( Note (..),
    readNoteDb,
    writeNoteDb,
    insertNote,
    lookupNote,
    NoteDb (..),
    deleteNote,
    toAscList,
    toDescList,
    MultiLock,
    newMultiLock,
    withWrite,
    withRead,
  )
where

import Control.Concurrent.STM
import Control.Exception (bracket_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Foldable qualified as Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortBy, sortOn)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Prelude hiding (id)

data Note = Note
  { id :: UUID,
    open_time :: ZonedTime,
    archive_time :: Maybe ZonedTime,
    body :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype NoteDb = NoteDb (HashMap UUID Note)

readNoteDb :: FilePath -> IO (Either String NoteDb)
readNoteDb fp = fmap fromList . traverse Aeson.eitherDecodeStrict . BS8.lines <$> BS.readFile fp
  where
    fromList :: [Note] -> NoteDb
    fromList = NoteDb . HashMap.fromList . fmap (\n -> (n.id, n))

toAscList :: NoteDb -> [Note]
toAscList (NoteDb db) = sortOn (\n -> zonedTimeToUTC n.open_time) $ Foldable.toList db

toDescList :: NoteDb -> [Note]
toDescList (NoteDb db) =
  fmap snd
    . sortBy (comparing $ Down . fst)
    . fmap (\n -> (zonedTimeToUTC n.open_time, n))
    $ Foldable.toList db

writeNoteDb :: FilePath -> NoteDb -> IO ()
writeNoteDb fp = BSL.writeFile fp . BSL8.unlines . fmap Aeson.encode . toAscList

lookupNote :: UUID -> NoteDb -> Maybe Note
lookupNote noteId (NoteDb db) = HashMap.lookup noteId db

insertNote :: Note -> NoteDb -> NoteDb
insertNote note (NoteDb db) = NoteDb $ HashMap.insert note.id note db

deleteNote :: UUID -> NoteDb -> NoteDb
deleteNote noteId (NoteDb db) = NoteDb $ HashMap.delete noteId db

parseTags :: Text -> [Text]
parseTags = mapMaybe parseTag . Text.words
  where
    parseTag :: Text -> Maybe Text
    parseTag word = case Text.uncons word of
      Just ('#', tag) -> pure tag
      _ -> Nothing

data MultiLock = MultiLock
  { _readLocks :: TVar (Map FilePath Int),
    _writeLocks :: TVar (Set FilePath)
  }

newMultiLock :: IO MultiLock
newMultiLock = do
  r <- newTVarIO mempty
  w <- newTVarIO mempty
  pure $ MultiLock r w

withRead :: MultiLock -> FilePath -> IO a -> IO a
withRead (MultiLock r w) path =
  bracket_
    ( atomically $ do
        readTVar w >>= check . not . Set.member path
        modifyTVar r $ Map.insertWith (+) path 1
    )
    (atomically $ modifyTVar r $ Map.update (\n -> if n > 1 then Just (n - 1) else Nothing) path)

withWrite :: MultiLock -> FilePath -> IO a -> IO a
withWrite (MultiLock r w) path =
  bracket_
    ( atomically $ do
        readTVar r >>= check . not . Map.member path
        w' <- readTVar w
        check $ not $ Set.member path w'
        writeTVar w $ Set.insert path w'
    )
    (atomically $ modifyTVar w $ Set.delete path)
