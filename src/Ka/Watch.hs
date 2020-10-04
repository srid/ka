module Ka.Watch
  ( directoryFilesContent,
  )
where

import Control.Monad.Fix (MonadFix)
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import Data.Time (NominalDiffTime)
import Reflex
import Reflex.FSNotify (FSEvent, watchDir)
import System.FSNotify (defaultConfig)
import qualified System.FSNotify as FSN
import System.FilePath (takeExtension, takeFileName)
import System.FilePattern.Directory (getDirectoryFiles)

-- | Get the contents of all the files in the given directory
--
-- Returns an event that fires when any of the files are updated or deleted. The
-- initial event fire will contain all the files and their contents, with the
-- subsequent updates containing the fsnotify events.
directoryFilesContent ::
  ( PerformEvent t m,
    PostBuild t m,
    TriggerEvent t m,
    MonadHold t m,
    MonadFix m,
    MonadIO (Performable m),
    MonadIO m
  ) =>
  FilePath ->
  String ->
  m (Event t (Map FilePath (Maybe Text)))
directoryFilesContent dirPath ext = do
  fileChangeE <- watchDirectory dirPath ext
  initE <-
    performEvent $
      ffor fileChangeE $
        Map.traverseWithKey $ \fp -> traverse $ \() -> do
          liftIO $ readFileText fp
  (restE, fire) <- newTriggerEvent
  liftIO $ do
    files <- getDirectoryFiles "." ["*" <> ext]
    xs <- flip traverse files $ \fp -> do
      s <- readFileText fp
      pure (fp, Just s)
    fire $ Map.fromList xs
  pure $
    leftmost [restE, initE]

watchDirectory ::
  ( PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadIO (Performable m),
    MonadHold t m,
    MonadFix m
  ) =>
  FilePath ->
  String ->
  m (Event t (Map FilePath (Maybe ())))
watchDirectory dirPath ext = do
  fsEvt <- watchDirWithDebounce 0.1 dirPath
  let fsEvt' = fforMaybe fsEvt $ \es -> do
        let es' = ffilter (\e -> takeExtension (FSN.eventPath e) == ext) es
        if null es' then Nothing else Just es'
  let evt = fmapMaybe (traverse noteFileStatusFromEvent) fsEvt'
  pure $ Map.fromList <$> evt

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

noteFileStatusFromEvent :: FSN.Event -> Maybe (FilePath, Maybe ())
noteFileStatusFromEvent evt = do
  -- Using `takeFileName` here (discarding the parent path elements), because
  -- we are watching the current directory. Effectively this gives us the
  -- relative path, which is just the filename as we glob over *.md.
  let fp = takeFileName $ FSN.eventPath evt
  pure $
    (fp,) $ case evt of
      FSN.Added _ _ _ ->
        Just ()
      FSN.Modified _ _ _ ->
        Just ()
      FSN.Removed _ _ _ ->
        Nothing
      FSN.Unknown _ _ _ ->
        -- Unknown is an event that fsnotify (haskell) doesn't know how to
        -- handle. Let's consider the file to be modified, just to be safe.
        Just ()

-- | Like @Data.List.nubBy@ but keeps the last occurence
nubByKeepLast :: (a -> a -> Bool) -> [a] -> [a]
nubByKeepLast f =
  reverse . nubBy f . reverse
