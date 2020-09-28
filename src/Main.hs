module Main where

import Control.Exception (catch, throwIO)
import qualified Data.Map.Strict as Map
import Ka.App (kaApp)
import qualified Ka.Database as Db
import Ka.Diff (Changed (..))
import Main.Utf8 (withUtf8)
import Reflex (Reflex (never), ffor)
import Reflex.Host.Headless (runHeadlessApp)
import Reflex.Network (networkView)
import System.Directory (createDirectoryIfMissing, removeFile, withCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)

main :: IO ()
main =
  withUtf8 $ do
    createDirectoryIfMissing True "./doc/.ka/output"
    withCurrentDirectory "./doc" $ do
      runHeadlessApp $ do
        -- Fire this to quit the app
        -- (e, fire) <- newTriggerEvent
        dbWithDiff <- kaApp
        void $
          networkView $
            ffor dbWithDiff $ \(db, _diff) -> liftIO $ do
              -- putStrLn $ "Diff: " <> show diff
              forM_ (Map.toList $ Db.outputFiles db) $ \(k, v) -> do
                case v of
                  Added s -> do
                    putStrLn $ "+ " <> k
                    writeFileText (".ka" </> "output" </> k) s
                  Modified s -> do
                    putStrLn $ "* " <> k
                    writeFileText (".ka" </> "output" </> k) s
                  Removed -> do
                    putStrLn $ "- " <> k
                    removeIfExists k
        pure never

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e