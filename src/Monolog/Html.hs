{-# LANGUAGE DataKinds #-}

module Monolog.Html
  ( mainWithConfig,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import Data.UUID (UUID, toString, toText)
import GHC.Generics (Generic)
import Monolog.File
import Monolog.Note
import Network.Wai.Handler.Warp qualified as Warp
import Servant
import Servant.HTML.Blaze (HTML)
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
    :<|> ReqBody '[FormUrlEncoded] NoteBody :> "archive" :> PostRedirect
    :<|> "reopen" :> PostRedirect

data NotesPage = NotesPage FilePath [Note]

instance ToMarkup NotesPage where
  toMarkup (NotesPage path db) =
    Html.docTypeHtml ! Html.dataAttribute "bs-theme" "dark" $ do
      Html.head $ do
        Html.meta ! Attr.name "viewport" ! Attr.content "width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"
        Html.link ! Attr.rel "stylesheet" ! Attr.href "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css"
        Html.link ! Attr.rel "stylesheet" ! Attr.href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css"
        Html.title $ Html.string path
      Html.body $ Html.div ! Attr.class_ "container" $ do
        Html.div ! Attr.class_ "row my-3" $ do
          h ! Attr.id "new" $ do
            Html.i ! Attr.class_ "bi bi-file-earmark" $ ""
          Html.form ! Attr.action (pathLink path []) ! Attr.method "post" $ do
            Html.div ! Attr.class_ "pb-3" $ textarea 5 ! Attr.required "" ! Attr.name "body" $ ""
            button ! Attr.class_ "btn btn-success" $ do
              Html.i ! Attr.class_ "bi bi-file-earmark-plus" $ ""
        Html.div ! Attr.id "notes" $ do
          h ! Attr.id "open" $ do
            Html.i ! Attr.class_ "bi bi-file-earmark-text" $ ""
          forM_ open $ \(t, uuid, body) ->
            card
              uuid
              ( do
                  Html.i ! Attr.class_ "bi bi-file-earmark-plus" $ ""
                  " "
                  fmtTime t
              )
              ( Html.form ! Attr.method "post" $ do
                  Html.div ! Attr.class_ "pb-3" $ textarea (length (Text.lines body) + 2) ! Attr.name "body" $ Html.text body
                  button ! Attr.class_ "btn btn-primary" ! Attr.formaction (noteLink path uuid []) $ do
                    Html.i ! Attr.class_ "bi bi-file-earmark-check" $ ""
                  button ! Attr.class_ "mx-1 btn btn-primary" ! Attr.formaction (noteLink path uuid ["/archive"]) $ do
                    Html.i ! Attr.class_ "bi bi-journal-arrow-down" $ ""
              )
          unless (null archived) $ do
            h ! Attr.id "archived" $ do
              Html.i ! Attr.class_ "bi bi-journal" $ ""
          forM_ archived $ \(t_open, t_archive, uuid, body) ->
            card
              uuid
              ( do
                  Html.i ! Attr.class_ "bi bi-file-earmark-plus" $ ""
                  " "
                  fmtTime t_open
                  Html.preEscapedText "&emsp;"
                  Html.i ! Attr.class_ "bi bi-journal" $ ""
                  " "
                  fmtTime t_archive
              )
              ( do
                  Html.div ! Attr.class_ "mb-3 font-monospace" ! Attr.style "white-space: pre-wrap" $ Html.text body
                  Html.form ! Attr.action (noteLink path uuid ["/reopen"]) ! Attr.method "post" $ do
                    button ! Attr.class_ "btn btn-outline-secondary btn-sm" ! Attr.type_ "submit" $ do
                      Html.i ! Attr.class_ "bi bi-journal-arrow-up" $ ""
              )
    where
      (open, archived) = splitNotes db
      card uuid hdr bdy = Html.div ! Attr.class_ "card my-3" ! Attr.id (Html.textValue $ toText uuid) $ do
        Html.div ! Attr.class_ "card-header" $ do
          Html.div $ Html.small ! Attr.class_ "text-muted" $ hdr
        Html.div ! Attr.class_ "card-body" $ bdy
      h = Html.h5 ! Attr.class_ "mt-5"
      button = Html.button ! Attr.type_ "submit"

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
splitNotes = partitionWith $ \n -> case n.archived of
  Nothing -> Left (n.created, n.id, n.body.toText)
  Just t -> Right (n.created, t, n.id, n.body.toText)

pathLink :: FilePath -> [Text] -> AttributeValue
pathLink path t = Html.textValue $ Text.concat $ "/" : Text.pack path : t

noteLink :: FilePath -> UUID -> [Text] -> AttributeValue
noteLink path uuid t = pathLink path ("/" : toText uuid : t)

fmtTime :: ZonedTime -> Html.Html
fmtTime t = Html.time $ Html.string $ formatTime defaultTimeLocale "%y-%m-%d %H:%M" t

mainWithConfig :: IO ()
mainWithConfig = do
  env <- newEnv
  Warp.run 8888 $ serve (Proxy @HtmlApi) (server env)

fixLineEndings :: Text -> Text
fixLineEndings = Text.replace "\r\n" "\n"

server :: Env -> Server HtmlApi
server env path = getNotes :<|> newNote :<|> noteApi
  where
    runApp :: App a -> Handler a
    runApp (ReaderT app) = liftIO (runExceptT $ app env) >>= either handle pure
      where
        handle :: FileError -> Handler a
        handle NoSuchFile = throwError err404
        handle NoSuchNote = throwError err404

    redirectTo :: String -> Handler Redirection
    redirectTo tgt = pure $ addHeader ('/' : path <> tgt) NoContent

    getNotes :: Handler NotesPage
    getNotes = NotesPage path <$> runApp (readFileDesc path)

    newNote :: NoteBody -> Handler Redirection
    newNote (NoteBody body) = do
      note <- liftIO $ fromPartial $ PartialNote Nothing Nothing Nothing (fixLineEndings body)
      runApp (insertNote path note)
      redirectTo "#new"

    noteApi :: UUID -> Server NoteApi
    noteApi noteId = putBody :<|> archivePutNote :<|> reopenNote
      where
        putBody :: NoteBody -> Handler Redirection
        putBody (NoteBody body') = do
          runApp $ updateBody path noteId body'
          redirectTo $ '#' : toString noteId

        archivePutNote :: NoteBody -> Handler Redirection
        archivePutNote (NoteBody body')
          | Text.null body' = do
              runApp $ deleteNote path noteId
              redirectTo "#new"
          | otherwise = do
              now <- liftIO getZonedTime
              runApp $ modifyNote path noteId $ \n -> n {archived = Just now, body = mkBody body'}
              redirectTo "#open"

        reopenNote :: Handler Redirection
        reopenNote = do
          runApp $ modifyNote path noteId $ \n -> n {archived = Nothing}
          redirectTo $ '#' : toString noteId

newtype NoteBody = NoteBody
  {body :: Text}
  deriving stock (Generic)
  deriving anyclass (FromForm)
