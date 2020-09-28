module Main where

import qualified Data.Map.Strict as Map
import Ka.App (kaApp)
import qualified Ka.Database as Db
import Main.Utf8 (withUtf8)
import Reflex (Reflex (never), ffor)
import Reflex.Host.Headless (runHeadlessApp)
import Reflex.Network (networkView)
import System.Directory (withCurrentDirectory)

main :: IO ()
main =
  withUtf8 $ do
    withCurrentDirectory "./doc" $ do
      runHeadlessApp $ do
        -- Fire this to quit the app
        -- (e, fire) <- newTriggerEvent
        dbWithDiff <- kaApp
        void $
          networkView $
            ffor dbWithDiff $ \(db, diff) -> liftIO $ do
              -- liftIO $ print st
              putStrLn $ "Diff: " <> show diff
              forM_ (Map.toList $ Db.outputFiles db) $ \(k, v) -> do
                putStr $ k <> " : "
                print v
              putStrLn "===\n"
        pure never
