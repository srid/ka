module Ka.Watch
  ( watchDirWithDebounce,
    readChangedFile,
  )
where

import Control.Monad.Fix (MonadFix)
import Data.List (nubBy)
import Data.Time (NominalDiffTime)
import Ka.Diff (Changed (..))
import Ka.Markdown (noteExtension)
import Reflex
  ( MonadHold,
    PerformEvent (Performable),
    PostBuild (..),
    Reflex (Event),
    TriggerEvent,
    batchOccurrences,
  )
import Reflex.FSNotify (FSEvent, watchDir)
import System.FSNotify (defaultConfig)
import qualified System.FSNotify as FSN
import System.FilePath (takeExtension, takeFileName)

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

readChangedFile :: FSN.Event -> IO (Maybe (FilePath, Changed Text))
readChangedFile evt =
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
    -- Using `takeFileName` here (discarding the parent path elements), because
    -- we are watching the current directory. Effectively this gives us the
    -- relative path, which is just the filename as we glob over *.md.
    fp = takeFileName $ FSN.eventPath evt

-- | Like @Data.List.nubBy@ but keeps the last occurence
nubByKeepLast :: (a -> a -> Bool) -> [a] -> [a]
nubByKeepLast f =
  reverse . nubBy f . reverse
