{-# LANGUAGE RecursiveDo #-}

module Ka.App where

import Control.Monad.Fix (MonadFix)
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import Data.Time (NominalDiffTime)
import Ka.Database (Ctx (Ctx), Db)
import qualified Ka.Database as Db
import Ka.Diff (Changed (..))
import Ka.Markdown (noteExtension, notePattern)
import Ka.Plugin.Markdown (markdownPlugin)
import Ka.Plugin.ViewGraph (viewGraphPlugin)
import Ka.Plugin.ViewNote (viewNotePlugin)
import Ka.Plugin.WikiLink (wikiLinkPlugin)
import Reflex
import Reflex.FSNotify (FSEvent, watchDir)
import Reflex.Host.Headless (MonadHeadlessApp)
import System.FSNotify (defaultConfig)
import qualified System.FSNotify as FSN
import System.FilePath (takeExtension, takeFileName)
import System.FilePattern.Directory (getDirectoryFiles)

ctx :: Ctx
ctx =
  Ctx
    [ markdownPlugin,
      wikiLinkPlugin,
      viewNotePlugin,
      viewGraphPlugin
    ]

-- | Like `watchDir` but batches file events
--
-- Returned event is in the form of list, which is guaranteed to not have repeat
-- events (i.e., 2 or more events with the same eventPath)
watchDirWithDebounce ::
  ( PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadIO (Performable m),
    MonadHold t m,
    MonadFix m
  ) =>
  NominalDiffTime ->
  FilePath ->
  m (Event t [FSEvent])
watchDirWithDebounce ms dirPath = do
  let cfg = defaultConfig {FSN.confDebounce = FSN.Debounce ms}
  pb <- getPostBuild
  evt <- watchDir cfg (dirPath <$ pb) (const True)
  evtGrouped <- fmap toList <$> batchOccurrences ms evt
  -- Discard all but the last event for each path.
  pure $ nubByKeepLast ((==) `on` FSN.eventPath) <$> evtGrouped

-- | Like @Data.List.nubBy@ but keeps the last occurence
nubByKeepLast :: (a -> a -> Bool) -> [a] -> [a]
nubByKeepLast f =
  reverse . nubBy f . reverse

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
            catMaybes <$> traverse noteChange xs
  rec dbWithChanges <-
        holdDyn (db0, mempty) $
          ffor (attach (current dbWithChanges) dbChanges) $ \((things, _), changes) ->
            (Db.changeDb ctx things changes, changes)
  pure dbWithChanges

noteChange :: FSN.Event -> IO (Maybe (FilePath, Changed Text))
noteChange evt =
  if takeExtension fp == noteExtension
    then fmap Just $ case evt of
      FSN.Added _ _ _ ->
        (fp,) . Added <$> readFileText fp
      FSN.Modified _ _ _ ->
        (fp,) . Modified <$> readFileText fp
      FSN.Removed _ _ _ ->
        pure (fp, Removed)
      FSN.Unknown _ _ _ ->
        -- Unknown is an event that fsnotify (haskell) doesn't know how to
        -- handle. Let's consider the file to be modified, just to be safe.
        (fp,) . Modified <$> readFileText fp
    else pure Nothing
  where
    fp = takeFileName $ FSN.eventPath evt
