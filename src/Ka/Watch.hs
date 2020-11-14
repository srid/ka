module Ka.Watch
  ( directoryFilesContent,
  )
where

import Control.Monad.Fix (MonadFix)
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import Data.Time (NominalDiffTime, UTCTime)
import Data.Traversable (for)
import Reflex
import Reflex.FSNotify (FSEvent, watchTree)
import System.Directory (getModificationTime, makeAbsolute)
import System.FSNotify (defaultConfig)
import qualified System.FSNotify as FSN
import System.FilePath (takeExtension)
import System.FilePath.Posix (makeRelative)
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
  m (Event t (Map FilePath (Maybe (UTCTime, Text))))
directoryFilesContent dirPathRel ext = do
  -- This function needs absolute directory path for `makeRelative` (see below)
  -- to work.
  dirPath <- liftIO $ makeAbsolute dirPathRel
  fileChangeE <- watchDirectory dirPath ext
  contentE <-
    performEvent $
      ffor fileChangeE $ \m ->
        -- fsnotify returns absolute paths; we must convert them back to
        -- relative paths, to be consistent with `content0E` (which uses
        -- getDirectoryFiles below to return relative paths)
        let toRel = makeRelative dirPath
         in flip Map.traverseWithKey (Map.mapKeys toRel m) $
              \fp -> traverse $ \mt -> do
                c <- liftIO $ readFileText fp
                pure (mt, c)
  -- Gather the current list of files as an event, and trigger it in the output
  -- event (see leftmost below)
  (content0E, fire) <- newTriggerEvent
  liftIO $ do
    files <- getDirectoryFiles "." ["**/*" <> ext]
    xs <- for files $ \fp -> do
      mt <- getModificationTime fp
      s <- readFileText fp
      pure (fp, Just (mt, s))
    fire $ Map.fromList xs
  pure $ leftmost [content0E, contentE]

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
  m (Event t (Map FilePath (Maybe UTCTime)))
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
  evt <- watchTree cfg (dirPath <$ pb) (const True)
  evtGrouped <- fmap toList <$> batchOccurrences ms evt
  -- Discard all but the last event for each path.
  pure $ nubByKeepLast ((==) `on` FSN.eventPath) <$> evtGrouped

noteFileStatusFromEvent :: FSN.Event -> Maybe (FilePath, Maybe UTCTime)
noteFileStatusFromEvent evt = do
  pure $
    (FSN.eventPath evt,) $ case evt of
      FSN.Added _ mt _ ->
        Just mt
      FSN.Modified _ mt _ ->
        Just mt
      FSN.Removed {} ->
        Nothing
      FSN.Unknown _ mt _ ->
        -- Unknown is an event that fsnotify (haskell) doesn't know how to
        -- handle. Let's consider the file to be modified, just to be safe.
        Just mt

-- | Like @Data.List.nubBy@ but keeps the last occurence
nubByKeepLast :: (a -> a -> Bool) -> [a] -> [a]
nubByKeepLast f =
  reverse . nubBy f . reverse
