module Main where

import Control.Exception (catch, throwIO)
import qualified Data.Map.Strict as Map
import Ka.App (kaApp)
import Ka.Watch (Status (..))
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
    createDirectoryIfMissing True $ notesDir </> ".ka" </> "output"
    withCurrentDirectory notesDir $ do
      runHeadlessApp $ do
        -- Fire this to quit the app
        -- (e, fire) <- newTriggerEvent
        output <- kaApp
        void $
          performEvent_ $
            ffor output $ \outputFiles -> liftIO $ do
              -- putStrLn $ "Diff: " <> show diff
              forM_ (Map.toList outputFiles) $ \(k, (st, genS)) -> do
                case st of
                  Untracked -> do
                    s <- genS
                    putStrLn $ "+ " <> k
                    writeFileBS (".ka" </> "output" </> k) s
                  Dirty -> do
                    s <- genS
                    putStrLn $ "* " <> k
                    writeFileBS (".ka" </> "output" </> k) s
                  Deleted -> do
                    putStrLn $ "- " <> k
                    removeIfExists k
        pure never

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e