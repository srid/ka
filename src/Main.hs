module Main where

import Control.Exception (catch, throwIO)
import qualified Data.Map.Strict as Map
import Ka.App (kaApp)
import Main.Utf8 (withUtf8)
import Reflex (Reflex (never), ffor, performEvent_)
import Reflex.Host.Headless (runHeadlessApp)
import System.Directory (createDirectoryIfMissing, removeFile, withCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

notesDir :: FilePath
notesDir = "/home/srid/Sync/zk"

main :: IO ()
main =
  withUtf8 $ do
    let outputDir = notesDir </> ".ka" </> "output"
    createDirectoryIfMissing True outputDir
    withCurrentDirectory notesDir $ do
      runHeadlessApp $ do
        evts <- kaApp
        forM_ evts $ \output ->
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