module Main where

import Control.Exception (catch, throwIO)
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ka.App (kaApp)
import Main.Utf8 (withUtf8)
import Reflex
import Reflex.Dom
import System.Directory (removeFile, withCurrentDirectory)
import System.IO.Error (isDoesNotExistError)

notesDir :: FilePath
notesDir = "/home/srid/Sync/zk"

main :: IO ()
main =
  withUtf8 $
    withCurrentDirectory notesDir $ do
      mainWidgetWithHead
        (el "title" $ text "ka Jsaddle")
        bodyWidget

bodyWidget ::
  ( DomBuilder t m,
    MonadIO m,
    PerformEvent t m,
    PostBuild t m,
    TriggerEvent t m,
    MonadHold t m,
    MonadFix m,
    MonadIO (Performable m)
  ) =>
  m ()
bodyWidget = do
  output <- kaApp
  text "We will show file updates here"
  widgetHold_ blank $
    ffor (Map.keys <$> output) $ \fs ->
      forM_ fs $ \fp -> do
        el "li" $ text $ T.pack fp

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e