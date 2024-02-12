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
import Text.Blaze.Html (Html, ToMarkup, (!))
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
    let (open, close) = splitNotes (toDescList db)
     in Html.docTypeHtml $ do
          Html.body $ do
            Html.form ! Attr.action (pathLink path []) ! Attr.method "post" $ do
              Html.textarea ! Attr.required "" ! Attr.name "body" $ ""
              Html.button ! Attr.type_ "submit" $ "Submit"
            Html.hr
            Html.div ! Attr.id "notes" $ do
              forM_ open $ \(t, uuid, body) ->
                Html.div ! Attr.id (Html.textValue $ toText uuid) $ do
                  Html.p . Html.string $ fmtTime t
                  Html.form ! Attr.method "post" $ do
                    Html.textarea ! Attr.required "" ! Attr.name "body" $ Html.text body
                    Html.button ! Attr.type_ "submit" ! Attr.formaction (noteLink path uuid []) $ "Save"
                    Html.button ! Attr.type_ "submit" ! Attr.formaction (noteLink path uuid ["/close"]) $ "Save & Close"
              unless (null open || null close) Html.hr
              forM_ close $ \(t_open, t_close, uuid, body) ->
                Html.div ! Attr.id (Html.textValue $ toText uuid) $ do
                  Html.p $ do
                    Html.string $ fmtTime t_open
                    Html.preEscapedText " &mdash; "
                    Html.string $ fmtTime t_close
                  Html.p $ Html.text body
                  Html.form ! Attr.action (noteLink path uuid ["/open"]) ! Attr.method "post" $ do
                    Html.button ! Attr.type_ "submit" $ "Reopen"

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

fmtTime :: ZonedTime -> String
fmtTime = formatTime defaultTimeLocale "%y-%m-%d %H:%M"

header :: Note -> Html
header note = do
  Html.string $ fmtTime note.open_time
  forM_ note.close_time $ \close -> do
    Html.preEscapedText " &mdash; "
    Html.string $ fmtTime close

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
