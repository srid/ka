{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Data.Map.Strict as Map
import Main.Utf8 (withUtf8)
import Reflex
import Reflex.FSNotify (watchDir)
import Reflex.Host.Headless (MonadHeadlessApp, runHeadlessApp)
import Reflex.Network (networkView)
import System.FSNotify (defaultConfig)
import qualified System.FSNotify as FSN
import System.FilePattern.Directory (getDirectoryFiles)

main :: IO ()
main =
  withUtf8 $ do
    runHeadlessApp $ do
      -- Fire this to quit the app
      -- (e, fire) <- newTriggerEvent
      kaApp
      pure never

kaApp :: MonadHeadlessApp t m => m ()
kaApp = do
  pb <- getPostBuild
  let inputDir = "." <$ pb
  changes <- watchDir defaultConfig inputDir (const True)
  fs0 :: Map FilePath Text <- fmap Map.fromList $ do
    liftIO $ do
      files <- getDirectoryFiles "." ["*.md"]
      forM files $ \fp ->
        (fp,) <$> readFileText fp
  fsE :: Event t (Map FilePath (Maybe Text)) <-
    performEvent $
      ffor changes $
        fmap one . \case
          FSN.Added fp _ _ -> (fp,) . Just <$> readFileText fp
          FSN.Modified fp _ _ -> (fp,) . Just <$> readFileText fp
          FSN.Removed fp _ _ -> pure (fp, Nothing)
          FSN.Unknown fp _ _ -> pure (fp, Nothing) -- ?
  rec fsWithDiff :: Dynamic t (Map FilePath Text, Map FilePath (Maybe Text)) <-
        holdDyn (fs0, mempty) $
          ffor (attach (current fsWithDiff) fsE) $ \((things, _lastChanged), changed) ->
            (patchMap things changed, changed)
  _fs <-
    holdUniqDyn
      =<< listHoldWithKey fs0 fsE (\_fp s -> pure s)
  void $
    networkView $
      ffor fsWithDiff $ \(things, changed) -> do
        liftIO $ print things
        liftIO $ print changed
        liftIO $ putStrLn "===\n"

patchMap :: Ord k => Map k a -> Map k (Maybe a) -> Map k a
patchMap xs diff =
  let (toAdd, toDel) = Map.mapEither (maybeToLeft ()) diff
   in Map.union toAdd xs `Map.difference` toDel