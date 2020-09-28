module Main where

import qualified Data.Map.Strict as Map
import Ka.App (kaApp)
import qualified Ka.Database as Db
import Main.Utf8 (withUtf8)
import Reflex (Reflex (never), ffor)
import Reflex.Host.Headless (runHeadlessApp)
import Reflex.Network (networkView)

main :: IO ()
main =
  withUtf8 $ do
    runHeadlessApp $ do
      -- Fire this to quit the app
      -- (e, fire) <- newTriggerEvent
      stWithDiff <- kaApp "./doc"
      void $
        networkView $
          ffor stWithDiff $ \(st, diff) -> liftIO $ do
            -- liftIO $ print st
            putStrLn $ "Diff: " <> show diff
            forM_ (Map.toList $ Db.outputFiles st) $ \(k, v) -> do
              putStr $ k <> " : "
              print v
            putStrLn "===\n"
      pure never
