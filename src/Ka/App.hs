{-# LANGUAGE RecursiveDo #-}

module Ka.App where

import qualified Data.Map.Strict as Map
import Ka.Database (Ctx (Ctx), Db)
import qualified Ka.Database as Db
import Ka.Diff (Changed (..))
import Ka.Markdown (notePattern)
import Ka.Plugin.Markdown (markdownPlugin)
import Ka.Plugin.ViewGraph (viewGraphPlugin)
import Ka.Plugin.ViewNote (viewNotePlugin)
import Ka.Plugin.WikiLink (wikiLinkPlugin)
import Ka.Watch (readChangedFile, watchDirWithDebounce)
import Reflex
import Reflex.Host.Headless (MonadHeadlessApp)
import System.FilePattern.Directory (getDirectoryFiles)

ctx :: Ctx
ctx =
  Ctx
    [ markdownPlugin,
      wikiLinkPlugin,
      viewNotePlugin,
      viewGraphPlugin
    ]

kaApp :: MonadHeadlessApp t m => m (Dynamic t (Db, Map FilePath (Changed Text)))
kaApp = do
  fileChanges <- watchDirWithDebounce 0.1 "."
  db0 :: Db <-
    Db.initDb ctx <$> do
      liftIO $ do
        files <- getDirectoryFiles "." [notePattern]
        fmap Map.fromList $
          forM files $ \fp -> do
            s <- readFileText fp
            pure (fp, s)
  dbChanges <-
    fmap (fmap Map.fromList) $
      performEvent $
        ffor fileChanges $ \(toList -> xs) ->
          liftIO $ do
            catMaybes <$> traverse readChangedFile xs
  rec dbWithChanges <-
        holdDyn (db0, mempty) $
          ffor (attach (current dbWithChanges) dbChanges) $ \((things, _), changes) ->
            (Db.changeDb ctx things changes, changes)
  pure dbWithChanges
