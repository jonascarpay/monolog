{-# LANGUAGE DataKinds #-}

module Api
  ( mainWithConfig,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
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
import Text.Blaze.Html (ToMarkup, (!))
import Text.Blaze.Html qualified as Html
import Text.Blaze.Html5 (AttributeValue)
import Text.Blaze.Html5 qualified as Html
import Text.Blaze.Html5.Attributes qualified as Attr
import Web.FormUrlEncoded (FromForm)
import Prelude hiding (id)

type HtmlApi = Capture "path" FilePath :> FileApi

type PostRedirect = Verb 'POST 303 '[JSON] Redirection

type Redirection = (Headers '[Header "Location" String] NoContent)

type FileApi =
  Get '[HTML] NotesPage
    :<|> ReqBody '[FormUrlEncoded] NoteBody :> PostRedirect
    :<|> (Capture "id" UUID :> NoteApi)

type NoteApi =
  ReqBody '[FormUrlEncoded] NoteBody :> PostRedirect
    :<|> "delete" :> PostRedirect
    :<|> ReqBody '[FormUrlEncoded] NoteBody :> "close" :> PostRedirect
    :<|> "open" :> PostRedirect

data NotesPage = NotesPage FilePath NoteDb

instance ToMarkup NotesPage where
  toMarkup (NotesPage path db) =
    Html.docTypeHtml ! Html.dataAttribute "bs-theme" "dark" $ do
      Html.head $ do
        Html.meta ! Attr.name "viewport" ! Attr.content "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
        Html.link ! Attr.rel "stylesheet" ! Attr.href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css"
        Html.link ! Attr.rel "stylesheet" ! Attr.href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
      Html.body $ Html.div ! Attr.class_ "container" $ do
        Html.div ! Attr.class_ "row my-3" $ do
          h "New"
          Html.form ! Attr.action (pathLink path []) ! Attr.method "post" $ do
            Html.div ! Attr.class_ "pb-3" $ textarea 5 ! Attr.required "" ! Attr.name "body" $ ""
            button $ do
              Html.i ! Attr.class_ "bi bi-journal-plus" $ ""
              " Create"
        Html.div ! Attr.id "notes" $ do
          h ! Attr.id "open" $ Html.string $ "Open (" <> show (length open) <> ")"
          forM_ open $ \(t, uuid, body) ->
            card
              uuid
              (Html.small $ fmtTime t)
              ( Html.form ! Attr.method "post" $ do
                  Html.div ! Attr.class_ "pb-3" $ textarea (length (Text.lines body) + 2) ! Attr.required "" ! Attr.name "body" $ Html.text body
                  Html.div ! Attr.class_ "row" $ do
                    Html.div ! Attr.class_ "col-6" $ do
                      button ! Attr.formaction (noteLink path uuid []) $ do
                        Html.i ! Attr.class_ "bi bi-journal-check" $ ""
                        " Save"
                    Html.div ! Attr.class_ "col-6" $ do
                      button ! Attr.formaction (noteLink path uuid ["/close"]) $ do
                        Html.i ! Attr.class_ "bi bi-journal-arrow-down" $ ""
                        " Save & Close"
              )
          unless (null closed) $ do
            h ! Attr.id "closed" $ Html.string $ "Closed (" <> show (length closed) <> ")"
          forM_ closed $ \(t_open, t_close, uuid, body) ->
            card
              uuid
              ( Html.small $ do
                  fmtTime t_open
                  Html.preEscapedText " &mdash; "
                  fmtTime t_close
              )
              ( do
                  Html.div ! Attr.class_ "font-monospace" ! Attr.style "white-space: pre-wrap" $ Html.text body
                  Html.form ! Attr.action (noteLink path uuid ["/open"]) ! Attr.method "post" $ do
                    button $ do
                      Html.i ! Attr.class_ "bi bi-journal-arrow-up" $ ""
                      " Reopen"
              )
    where
      (open, closed) = splitNotes (toDescList db)
      card uuid hdr bdy = Html.div ! Attr.class_ "card my-3" ! Attr.id (Html.textValue $ toText uuid) $ do
        Html.div ! Attr.class_ "card-header" $ hdr
        Html.div ! Attr.class_ "card-body" $ bdy
      h = Html.h5 ! Attr.class_ "mt-5"
      button = Html.button ! Attr.class_ "btn btn-outline-primary btn-sm w-100" ! Attr.type_ "submit"

      textarea :: Int -> Html.Html -> Html.Html
      textarea rows = Html.textarea ! Attr.class_ "form-control font-monospace" ! Attr.rows (Html.stringValue $ show rows)

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith _ [] = ([], [])
partitionWith f (x : xs) = case f x of
  Left b -> (b : bs, cs)
  Right c -> (bs, c : cs)
  where
    (bs, cs) = partitionWith f xs

splitNotes :: [Note] -> ([(ZonedTime, UUID, Text)], [(ZonedTime, ZonedTime, UUID, Text)])
splitNotes = partitionWith $ \n -> case n.close_time of
  Nothing -> Left (n.open_time, n.id, n.body)
  Just t -> Right (n.open_time, t, n.id, n.body)

pathLink :: FilePath -> [Text] -> AttributeValue
pathLink path tail = Html.textValue $ Text.concat $ "/" : Text.pack path : tail

noteLink :: FilePath -> UUID -> [Text] -> AttributeValue
noteLink path uuid tail = pathLink path ("/" : toText uuid : tail)

fmtTime :: ZonedTime -> Html.Html
fmtTime t = Html.time $ Html.string $ formatTime defaultTimeLocale "%y-%m-%d %H:%M" t

mainWithConfig :: IO ()
mainWithConfig = do
  lock <- newMultiLock
  Warp.run 8888 $ serve (Proxy @HtmlApi) (server (Env lock))

newtype Env = Env MultiLock

server :: Env -> Server HtmlApi
server (Env lock) path = getNotes :<|> newNote :<|> noteApi
  where
    redirect :: Handler Redirection
    redirect = pure $ addHeader ('/' : path) NoContent

    readDb :: Handler NoteDb
    readDb = do
      exists <- liftIO $ doesFileExist path
      unless exists $ throwError err404
      liftIO (withRead lock path $ readNoteDb path) >>= \case
        Left err -> throwError err500 {errBody = BSL8.pack err}
        Right db -> pure db

    writeDb :: NoteDb -> Handler ()
    writeDb db = liftIO $ withWrite lock path $ writeNoteDb path db

    getNotes :: Handler NotesPage
    getNotes = NotesPage path <$> readDb

    newNote :: NoteBody -> Handler Redirection
    newNote (NoteBody body) = do
      db <- readDb
      note <- liftIO $ do
        uuid <- nextRandom
        now <- getZonedTime
        pure $ Note uuid now Nothing body
      writeDb $ insertNote note db
      redirect

    noteApi :: UUID -> Server NoteApi
    noteApi noteId = putBody :<|> deleteNote' :<|> closePutNote :<|> openNote
      where
        putBody :: NoteBody -> Handler Redirection
        putBody (NoteBody body') = do
          db <- readDb
          case lookupNote noteId db of
            Nothing -> throwError err404
            Just note -> do
              let note' = note {body = body'} :: Note
              writeDb $ insertNote note' db
              redirect

        deleteNote' :: Handler Redirection
        deleteNote' = do
          db <- readDb
          writeDb $ deleteNote noteId db
          redirect

        closePutNote :: NoteBody -> Handler Redirection
        closePutNote (NoteBody body') = do
          now <- liftIO getZonedTime
          db <- readDb
          case lookupNote noteId db of
            Nothing -> throwError err404
            Just note -> do
              let note' = (note {body = body', close_time = Just now} :: Note)
              writeDb $ insertNote note' db
              redirect

        openNote :: Handler Redirection
        openNote = do
          db <- readDb
          case lookupNote noteId db of
            Nothing -> throwError err404
            Just note -> do
              let note' = (note {close_time = Nothing} :: Note)
              writeDb $ insertNote note' db
              redirect

newtype NoteBody = NoteBody
  {body :: Text}
  deriving stock (Generic)
  deriving anyclass (FromForm)
