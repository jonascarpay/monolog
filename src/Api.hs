{-# LANGUAGE DataKinds #-}

module Api
  ( mainWithConfig,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON (..))
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp qualified as Warp
import NoteDb
import Servant
import Servant.HTML.Blaze (HTML)
import System.Directory
import Text.Blaze.Html (Attribute, Html, ToMarkup, (!))
import Text.Blaze.Html qualified as Html
import Text.Blaze.Html5 (AttributeValue)
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr
import Web.FormUrlEncoded (FromForm)
import Prelude hiding (id)

type HtmxApi = Capture "path" FilePath :> FileApi

type FileApi =
  Get '[HTML] (Page NotesPage)
    :<|> ReqBody '[FormUrlEncoded] Body :> Post '[HTML] NoteView
    :<|> (Capture "id" UUID :> NoteApi)

type NoteApi =
  Get '[HTML] NoteView
    :<|> ReqBody '[FormUrlEncoded] Body :> Post '[HTML] NoteView
    :<|> Delete '[HTML] Empty
    :<|> "edit" :> Get '[HTML] NoteEdit
    :<|> "close" :> Post '[HTML] NoteView
    :<|> "open" :> Post '[HTML] NoteView

data NotesPage = NotesPage FilePath NoteDb

-- instance ToJSON NotesPage where
--   toJSON (NotesPage path db) = toJSON $ toList db

instance ToMarkup NotesPage where
  toMarkup (NotesPage path db) = do
    Html.form ! hxPost (pathLink path []) ! hxTarget "#notes" ! hxSwap "afterbegin" $ do
      Html.textarea ! Attr.required "" ! Attr.name "body" $ ""
      Html.button ! Attr.type_ "submit" $ "Submit"
    Html.hr
    Html.div ! Attr.id "notes" $
      forM_ (toDescList db) $
        \note -> Html.toMarkup (NoteView path note)

data NoteView = NoteView FilePath Note

pathLink :: FilePath -> [Text] -> AttributeValue
pathLink path tail = Html.textValue $ Text.concat $ "/" : Text.pack path : tail

noteLink :: NoteView -> [Text] -> AttributeValue
noteLink (NoteView path note) tail = pathLink path ("/" : toText note.id : tail)

fmtTime :: ZonedTime -> String
fmtTime = formatTime defaultTimeLocale "%y-%m-%d %H:%M"

header :: Note -> Html
header note = do
  Html.string $ fmtTime note.open_time
  forM_ note.close_time $ \close -> do
    Html.preEscapedText " &mdash; "
    Html.string $ fmtTime close

newtype Page p = Page p

instance (ToMarkup a) => ToMarkup (Page a) where
  toMarkup (Page content) =
    Html.docTypeHtml $ do
      Html.head $ Html.script ! Attr.src "https://unpkg.com/htmx.org@1.9.10" $ ""
      Html.body $ Html.toMarkup content

instance ToMarkup NoteView where
  toMarkup vw@(NoteView path note) = Html.div ! hxTarget "this" ! hxSwap "outerHTML" $ do
    Html.p $ header note
    Html.p $ Html.text note.body
    Html.p $
      case note.close_time of
        Nothing -> do
          Html.button ! hxGet (noteLink vw ["/edit"]) $ "Edit"
          Html.button ! hxPost (noteLink vw ["/close"]) $ "Close"
        Just _ ->
          Html.button ! hxPost (noteLink vw ["/open"]) $ "Reopen"

newtype NoteEdit = NoteEdit NoteView

instance ToMarkup NoteEdit where
  toMarkup (NoteEdit vw@(NoteView path note)) = Html.div ! hxTarget "this" ! hxSwap "outerHTML" $
    Html.form ! hxPost (noteLink vw []) $ do
      Html.p $ header note
      Html.p $ Html.textarea ! Attr.name "body" $ Html.text note.body
      Html.p $ do
        Html.button ! Attr.type_ "submit" $ "Save changes"
        Html.button ! hxGet (noteLink vw []) $ "Discard changes"
        Html.button ! hxDelete (noteLink vw []) $ "Delete"

hxTarget, hxSwap, hxGet, hxPost, hxDelete :: AttributeValue -> Attribute
hxTarget = Html.dataAttribute "hx-target"
hxSwap = Html.dataAttribute "hx-swap"
hxGet = Html.dataAttribute "hx-get"
hxPost = Html.dataAttribute "hx-post"
hxDelete = Html.dataAttribute "hx-delete"

data Empty = Empty

instance ToMarkup Empty where
  toMarkup _ = mempty

mainWithConfig :: IO ()
mainWithConfig = do
  lock <- newMultiLock
  Warp.run 8888 $ serve (Proxy @HtmxApi) (server (Env lock))

newtype Env = Env MultiLock

server :: Env -> Server HtmxApi
server (Env lock) path = getNotes :<|> newNote :<|> noteApi
  where
    readDb :: Handler NoteDb
    readDb = do
      exists <- liftIO $ doesFileExist path
      unless exists $ throwError err404
      liftIO (withRead lock path $ readNoteDb path) >>= \case
        Left err -> throwError err500 {errBody = BSL8.pack err}
        Right db -> pure db

    writeDb :: NoteDb -> Handler ()
    writeDb db = liftIO $ withWrite lock path $ writeNoteDb path db

    getNotes :: Handler (Page NotesPage)
    getNotes = Page . NotesPage path <$> readDb

    newNote :: Body -> Handler NoteView
    newNote (Body body) = do
      db <- readDb
      note <- liftIO $ do
        uuid <- nextRandom
        now <- getZonedTime
        pure $ Note uuid now Nothing body
      writeDb $ insertNote note db
      pure $ NoteView path note

    noteApi :: UUID -> Server NoteApi
    noteApi noteId = getNote :<|> updateBody :<|> deleteNote' :<|> getEditForm :<|> closeNote :<|> openNote
      where
        getNote :: Handler NoteView
        getNote = do
          db <- readDb
          case lookupNote noteId db of
            Nothing -> throwError err404
            Just note -> pure $ NoteView path note

        getEditForm :: Handler NoteEdit
        getEditForm = NoteEdit <$> getNote

        updateBody :: Body -> Handler NoteView
        updateBody (Body body) = do
          db <- readDb
          case lookupNote noteId db of
            Nothing -> throwError err404
            Just note -> do
              let note' = note {body = body} :: Note
              writeDb $ insertNote note' db
              pure $ NoteView path note'
        deleteNote' :: Handler Empty
        deleteNote' = do
          db <- readDb
          writeDb $ deleteNote noteId db
          pure Empty
        closeNote :: Handler NoteView
        closeNote = do
          now <- liftIO getZonedTime
          db <- readDb
          case lookupNote noteId db of
            Nothing -> throwError err404
            Just note -> do
              let note' = (note {close_time = Just now} :: Note)
              writeDb $ insertNote note' db
              pure $ NoteView path note'
        openNote :: Handler NoteView
        openNote = do
          db <- readDb
          case lookupNote noteId db of
            Nothing -> throwError err404
            Just note -> do
              let note' = (note {close_time = Nothing} :: Note)
              writeDb $ insertNote note' db
              pure $ NoteView path note'

newtype Body = Body
  { body :: Text
  }
  deriving stock (Generic)
  deriving anyclass (FromForm)

data NoteRequest = NoteRequest
  { id :: Maybe UUID,
    open_time :: Maybe ZonedTime,
    close_time :: Maybe ZonedTime,
    body :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, FromForm)

fromRequest :: NoteRequest -> IO Note
fromRequest req = do
  id <- maybe nextRandom pure req.id
  open_time <- maybe getZonedTime pure req.open_time
  let close_time = Nothing
  let body = req.body
  pure $ Note {..}
