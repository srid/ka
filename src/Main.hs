module Main where

import qualified Data.Map.Strict as Map
import Main.Utf8 (withUtf8)
import Reflex
import Reflex.FSNotify (watchDir)
import Reflex.Host.Headless (runHeadlessApp)
import Reflex.Network (networkView)
import System.FSNotify (Event (..), defaultConfig)
import System.FilePattern.Directory (getDirectoryFiles)

main :: IO ()
main =
  withUtf8 $ do
    putStrLn "App..."
    runHeadlessApp $ do
      -- Fire this to quit the app
      -- (e, fire) <- newTriggerEvent
      pb <- getPostBuild
      let inputDir = "." <$ pb
      changes <- watchDir defaultConfig inputDir (const True)
      fs0 <- fmap (Map.fromList . fmap (,())) $ liftIO $ getDirectoryFiles "." ["*.md"]
      let fsE =
            ffor changes $
              one . \case
                Added fp _ _ -> (fp, Just ())
                Modified fp _ _ -> (fp, Just ())
                Removed fp _ _ -> (fp, Nothing)
                Unknown fp _ _ -> (fp, Just ()) -- ?
      fs <-
        holdUniqDyn
          =<< listHoldWithKey fs0 fsE (\fp () -> liftIO $ readFileText fp)
      void $
        networkView $
          ffor fs $ \x ->
            liftIO $ print x
      pure never
