module Monolog.File
  ( insertNote,
    lookupNote,
    deleteNote,
    modifyNote,
    updateBody,
    updateArchived,
    readFileDesc,
    Env,
    App,
    FileError (..),
    newEnv,
  )
where

import Control.Concurrent.STM
import Control.Exception.Lifted (bracket_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Coerce (coerce)
import Data.Foldable qualified as Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (sortBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy.IO qualified as Lazy
import Data.Time
import Data.UUID (UUID)
import Monolog.Text (parseText, printText)
import Monolog.Types
import System.Directory (doesFileExist)

newtype NoteDb = NoteDb {unNoteDb :: HashMap UUID Note}

data FileError
  = NoSuchFile
  | NoSuchNote

type App a = ReaderT Env (ExceptT FileError IO) a

toAscList :: NoteDb -> [Note]
toAscList (NoteDb db) = sortOn (\n -> zonedTimeToUTC n.created) $ Foldable.toList db

readNoteDb :: FilePath -> IO NoteDb
readNoteDb path = do
  txt <- Lazy.readFile path
  let pnotes = parseText txt
  notes <- traverse fromPartial pnotes
  pure $ fromList notes

writeNoteDb :: FilePath -> NoteDb -> IO ()
writeNoteDb fp = Lazy.writeFile fp . printText . toAscList

readFileDesc :: FilePath -> App [Note]
readFileDesc fp = do
  NoteDb db <- readDb fp
  pure
    $ fmap snd
      . sortBy (comparing $ Down . fst)
      . fmap (\n -> (zonedTimeToUTC n.created, n))
      . Foldable.toList
    $ db

lookupNote :: FilePath -> UUID -> App Note
lookupNote fp uuid = do
  NoteDb db <- readDb fp
  case HashMap.lookup uuid db of
    Nothing -> lift $ throwE NoSuchNote
    Just n -> pure n

-- TODO don't export
insertNote :: FilePath -> Note -> App ()
insertNote fp note = withWrite fp $ coerce . HashMap.insert note.id note . coerce

{-# INLINE modifyNote #-}
modifyNote :: FilePath -> UUID -> (Note -> Note) -> App ()
modifyNote fp uuid f = withWriteM fp $ \(NoteDb db) ->
  case HashMap.lookup uuid db of
    Nothing -> throwError NoSuchNote
    Just note -> pure $ NoteDb $ HashMap.insert uuid (f note) db

-- lookupNote fp uuid >>= \case
--   Just n -> insertNote fp (f n)

updateBody :: FilePath -> UUID -> Text -> App ()
updateBody fp uuid body = modifyNote fp uuid $ \n -> mkNote uuid n.created n.archived body

updateArchived :: FilePath -> UUID -> Maybe ZonedTime -> App ()
updateArchived fp uuid t = modifyNote fp uuid $ \n -> n {archived = t}

deleteNote :: FilePath -> UUID -> App ()
deleteNote fp uuid = withWrite fp (NoteDb . HashMap.delete uuid . coerce)

data Env = Env
  { _readLocks :: TVar (Map FilePath Int),
    _writeLocks :: TVar (Set FilePath)
  }

newEnv :: IO Env
newEnv = do
  r <- newTVarIO mempty
  w <- newTVarIO mempty
  pure $ Env r w

fromList :: [Note] -> NoteDb
fromList = NoteDb . HashMap.fromList . fmap (\n -> (n.id, n))

readDb :: FilePath -> App NoteDb
readDb path = do
  Env r w <- ask
  bracket_
    ( liftIO . atomically $ do
        readTVar w >>= check . not . Set.member path
        modifyTVar r $ Map.insertWith (+) path 1
    )
    (liftIO . atomically $ modifyTVar r $ Map.update (\n -> if n > 1 then Just (n - 1) else Nothing) path)
    $ liftIO (doesFileExist path) >>= \case
      True -> liftIO $ readNoteDb path
      False -> throwError NoSuchFile

withWrite :: FilePath -> (NoteDb -> NoteDb) -> App ()
withWrite path k = withWriteM path (pure . k)

withWriteM :: FilePath -> (NoteDb -> App NoteDb) -> App ()
withWriteM path k = do
  Env r w <- ask
  bracket_
    ( liftIO . atomically $ do
        readTVar r >>= check . not . Map.member path
        w' <- readTVar w
        check $ not $ Set.member path w'
        writeTVar w $ Set.insert path w'
    )
    (liftIO $ atomically $ modifyTVar w $ Set.delete path)
    $ liftIO (doesFileExist path) >>= \case
      True -> do
        db <- liftIO $ readNoteDb path
        db' <- k db
        liftIO $ writeNoteDb path db'
      False -> throwError NoSuchFile
