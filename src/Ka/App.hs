{-# LANGUAGE RecursiveDo #-}

module Ka.App where

import qualified Data.Map.Strict as Map
import Ka.Database (Ctx (Ctx), Db)
import qualified Ka.Database as Db
import Ka.Diff (Changed (..))
import Ka.Plugin (wipPlugin)
import Ka.Plugin.Markdown (markdownPlugin)
import Ka.Plugin.WikiLink (wikiLinkPlugin)
import Reflex
import Reflex.FSNotify (watchDir)
import Reflex.Host.Headless (MonadHeadlessApp)
import System.FSNotify (defaultConfig)
import qualified System.FSNotify as FSN
import System.FilePath (takeExtension, takeFileName)
import System.FilePattern.Directory (FilePattern, getDirectoryFiles)

ctx :: Ctx
ctx =
  Ctx
    [ markdownPlugin,
      wikiLinkPlugin,
      wipPlugin
    ]

kaApp :: MonadHeadlessApp t m => m (Dynamic t (Db, Map FilePath (Changed Text)))
kaApp = do
  pb <- getPostBuild
  fileChanges <- watchDir defaultConfig ("." <$ pb) (const True)
  db0 :: Db <-
    Db.initDb ctx <$> do
      liftIO $ do
        files <- getDirectoryFiles "." [notePattern]
        fmap Map.fromList $
          forM files $ \fp -> do
            s <- readFileText fp
            pure (fp, s)
  dbChanges <-
    fmap (fmapMaybe id) $
      performEvent $
        -- TODO: group events
        ffor fileChanges $ \x -> liftIO $ do
          noteChange x >>= \case
            Nothing -> pure Nothing
            Just (fp, change) ->
              pure $ Just $ one (fp, change)
  rec dbWithChanges <-
        holdDyn (db0, mempty) $
          ffor (attach (current dbWithChanges) dbChanges) $ \((things, _), changes) ->
            (Db.changeDb ctx things changes, changes)
  pure dbWithChanges

noteExtension :: String
noteExtension = ".md"

notePattern :: FilePattern
notePattern = "*" <> noteExtension

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
        pure (fp, Removed) -- FIXME: ?
    else pure Nothing
  where
    fp = takeFileName $ FSN.eventPath evt
