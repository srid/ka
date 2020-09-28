{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import qualified Data.Map.Strict as Map
import Ka.Diff
import Ka.Plugin
import Main.Utf8 (withUtf8)
import Reflex
import Reflex.FSNotify (watchDir)
import Reflex.Host.Headless (MonadHeadlessApp, runHeadlessApp)
import Reflex.Network (networkView)
import System.FSNotify (defaultConfig)
import qualified System.FSNotify as FSN
import System.FilePath ((</>))
import System.FilePattern.Directory (getDirectoryFiles)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc (Pandoc))

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
            forM_ (Map.toList $ outputFiles st) $ \(k, v) -> do
              putStr $ k <> " : "
              print v
            putStrLn "===\n"
      pure never

kaApp :: MonadHeadlessApp t m => FilePath -> m (Dynamic t (KaState, KaStateDiff))
kaApp inputDir = do
  pb <- getPostBuild
  changes <- watchDir defaultConfig (inputDir <$ pb) (const True)
  st0 :: KaState <- fmap kaInit $ do
    liftIO $ do
      files <- getDirectoryFiles inputDir ["*.md"]
      fmap Map.fromList $
        forM files $ \fp -> do
          s <- readFileText (inputDir </> fp)
          pure (fp, s)
  diffE :: Event t KaStateDiff <-
    performEvent $
      ffor changes $
        -- TODO: group events
        -- TODO: filter by *.md
        fmap one . \case
          FSN.Added fp _ _ -> (fp,) . Added <$> readFileText fp
          FSN.Modified fp _ _ -> (fp,) . Modified <$> readFileText fp
          FSN.Removed fp _ _ -> pure (fp, Removed)
          FSN.Unknown fp _ _ -> pure (fp, Removed) -- FIXME: ?
  rec stWithDiff :: Dynamic t (KaState, KaStateDiff) <-
        holdDyn (st0, mempty) $
          ffor (attach (current stWithDiff) diffE) $ \((things, _lastChanged), changed) ->
            (kaPatch things changed, changed)
  pure stWithDiff

-- ka types
-- --------

data KaState = KaState
  { -- | Pandoc AST of plain-text files
    inputDoc :: Map FilePath (V Pandoc),
    -- | Outgoing links (used for building the graph)
    outLinks :: Map FilePath (V [FilePath]),
    -- | Graph of notes
    graph :: (),
    -- | Post-transformed Pandoc AST
    outputDoc :: Map FilePath (V Pandoc),
    -- | Pages to generate
    outputFiles :: Map FilePath (Change ByteString)
  }
  deriving (Show)

type KaStateDiff = Map FilePath (Change Text)

emptyKaState :: KaState
emptyKaState =
  KaState mempty mempty () mempty mempty

kaInit :: Map FilePath Text -> KaState
kaInit =
  kaPatch emptyKaState . fmap Added

-- Incremental update of ka state
-- ------------------------------

kaReset :: KaState -> KaState
kaReset st =
  st
    { inputDoc = Map.mapMaybe id $ fmap markSame $ inputDoc st,
      outLinks = Map.mapMaybe id $ fmap markSame $ outLinks st,
      outputDoc = Map.mapMaybe id $ fmap markSame $ outputDoc st,
      outputFiles = mempty
    }

kaPatch :: KaState -> KaStateDiff -> KaState
kaPatch (kaReset -> st) diff =
  let plugins = [demoPlugin]
      cmSpec = commonmarkSpec `foldMap` plugins
      diffDoc = Map.mapWithKey (fmap . parseMarkdown cmSpec) diff
      diffOutLinks = Map.mapWithKey (fmap . queryLinks) diffDoc
      inputDoc' = applyDiff diffDoc $ inputDoc st
      outLinks' = applyDiff diffOutLinks $ outLinks st
      outputFiles' :: Map FilePath (Change ByteString) =
        flip foldMap (fileGenerator <$> plugins) $ \gen ->
          gen inputDoc'
   in st
        { inputDoc = inputDoc',
          outLinks = outLinks',
          graph = (),
          outputDoc = inputDoc',
          outputFiles = outputFiles'
        }

queryLinks :: FilePath -> Pandoc -> [FilePath]
queryLinks _ _ = [] -- TODO

parseMarkdown :: CMSyntaxSpec -> FilePath -> Text -> Pandoc
parseMarkdown spec fp s =
  either (error . show) toPandoc $ join $ CM.commonmarkWith spec fp s
  where
    toPandoc = Pandoc mempty . B.toList . CP.unCm
