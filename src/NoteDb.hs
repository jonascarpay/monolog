module NoteDb
  ( Note (..),
    PartialNote (..),
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
    fromPartial,
    fromList,
  )
where

import Control.Concurrent.STM
import Control.Exception (bracket_)
import Data.Aeson (FromJSON, ToJSON)
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
import Data.UUID.V4 qualified as UUID
import GHC.Generics (Generic)
import Prelude hiding (id)

data Note = Note
  { id :: UUID,
    open_time :: ZonedTime,
    archive_time :: Maybe ZonedTime,
    body :: Text -- TODO newtype to guarantuee stripping
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

data PartialNote = PartialNote
  { id :: Maybe UUID,
    open_time :: Maybe ZonedTime,
    archive_time :: Maybe ZonedTime,
    body :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

fromPartial :: PartialNote -> IO Note
fromPartial (PartialNote pid popen archive_time body) = do
  id <- maybe UUID.nextRandom pure pid
  open_time <- maybe getZonedTime pure popen
  pure $ Note id open_time archive_time body

newtype NoteDb = NoteDb (HashMap UUID Note)

toAscList :: NoteDb -> [Note]
toAscList (NoteDb db) = sortOn (\n -> zonedTimeToUTC n.open_time) $ Foldable.toList db

toDescList :: NoteDb -> [Note]
toDescList (NoteDb db) =
  fmap snd
    . sortBy (comparing $ Down . fst)
    . fmap (\n -> (zonedTimeToUTC n.open_time, n))
    $ Foldable.toList db

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

fromList :: [Note] -> NoteDb
fromList = NoteDb . HashMap.fromList . fmap (\n -> (n.id, n))

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
