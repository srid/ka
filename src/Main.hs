module Main where

import Control.Exception (catch, throwIO)
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ka.App (kaApp)
import Main.Utf8 (withUtf8)
import Reflex
import Reflex.Dom
import Reflex.Host.Headless (runHeadlessApp)
import System.Directory (createDirectoryIfMissing, removeFile, withCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

notesDir :: FilePath
notesDir = "/home/srid/Sync/zk"

main :: IO ()
main =
  withCurrentDirectory notesDir $ do
    mainWidget bodyWidget

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

oldMain :: IO ()
oldMain =
  withUtf8 $ do
    let outputDir = notesDir </> ".ka" </> "output"
    createDirectoryIfMissing True outputDir
    withCurrentDirectory notesDir $ do
      runHeadlessApp $ do
        output <- kaApp
        void $
          performEvent_ $
            ffor output $ \outputFiles -> liftIO $ do
              -- putStrLn $ "Diff: " <> show diff
              forM_ (Map.toList outputFiles) $ \(k, mGenS) -> do
                case mGenS of
                  Nothing -> do
                    putStrLn $ "- " <> k
                    removeIfExists $ outputDir </> k
                  Just genS -> do
                    putStrLn $ "W " <> k
                    s <- genS
                    writeFileBS (outputDir </> k) $! s
        pure never

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e