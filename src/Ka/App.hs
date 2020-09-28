{-# LANGUAGE RecursiveDo #-}

module Ka.App where

import qualified Data.Map.Strict as Map
import Ka.Database (Ctx (Ctx), Db, initDb, patchDb)
import Ka.Diff (Changed (..))
import Ka.Plugin (demoPlugin)
import Reflex
import Reflex.FSNotify (watchDir)
import Reflex.Host.Headless (MonadHeadlessApp)
import System.FSNotify (defaultConfig)
import qualified System.FSNotify as FSN
import System.FilePath ((</>))
import System.FilePattern.Directory (getDirectoryFiles)

kaApp :: MonadHeadlessApp t m => FilePath -> m (Dynamic t (Db, Map FilePath (Changed Text)))
kaApp inputDir = do
  pb <- getPostBuild
  changes <- watchDir defaultConfig (inputDir <$ pb) (const True)
  let ctx = Ctx [demoPlugin]
  st0 :: Db <-
    initDb ctx <$> do
      liftIO $ do
        files <- getDirectoryFiles inputDir ["*.md"]
        fmap Map.fromList $
          forM files $ \fp -> do
            s <- readFileText (inputDir </> fp)
            pure (fp, s)
  diffE <-
    performEvent $
      ffor changes $
        -- TODO: group events
        -- TODO: filter by *.md
        fmap one . \case
          FSN.Added fp _ _ -> (fp,) . Added <$> readFileText fp
          FSN.Modified fp _ _ -> (fp,) . Modified <$> readFileText fp
          FSN.Removed fp _ _ -> pure (fp, Removed)
          FSN.Unknown fp _ _ -> pure (fp, Removed) -- FIXME: ?
  rec stWithDiff <-
        holdDyn (st0, mempty) $
          ffor (attach (current stWithDiff) diffE) $ \((things, _lastChanged), changed) ->
            (patchDb ctx things changed, changed)
  pure stWithDiff
