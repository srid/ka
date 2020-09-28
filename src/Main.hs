{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import qualified Data.Map.Strict as Map
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

main :: IO ()
main =
  withUtf8 $ do
    runHeadlessApp $ do
      -- Fire this to quit the app
      -- (e, fire) <- newTriggerEvent
      stWithDiff <- kaApp "./doc"
      void $
        networkView $
          ffor stWithDiff $ \(st, diff) -> do
            liftIO $ print st
            liftIO $ print diff
            liftIO $ putStrLn "===\n"

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

type FileContent = Text

type FileDoc = Either CM.ParseError (CP.Cm () B.Blocks)

-- | A value along with its diff status if any.
data V a
  = -- | The value a has not been modified
    VSame a
  | -- | The value was modified, added or deleted
    VChanged (Change a)
  deriving (Eq, Show)

data Change a
  = Added a
  | Modified a
  | Removed
  deriving stock (Eq, Show)
  deriving stock (Functor)

applyDiff :: Ord k => Map k (Change a) -> Map k (V a) -> Map k (V a)
applyDiff diff =
  Map.union (fmap VChanged diff)

data KaState = KaState
  { -- | Pandoc AST of plain-text files
    inputDoc :: Map FilePath (V FileDoc),
    -- | Outgoing links (used for building the graph)
    outLinks :: Map FilePath (V [FilePath]),
    -- | Graph of notes
    graph :: (),
    -- | Post-transformed Pandoc AST
    outputDoc :: Map FilePath (V FileDoc),
    -- | Pages to generate
    outputFiles :: Map FilePath (V ByteString)
  }
  deriving (Show)

type KaStateDiff = Map FilePath (Change FileContent)

emptyKaState :: KaState
emptyKaState =
  KaState mempty mempty () mempty mempty

kaInit :: Map FilePath FileContent -> KaState
kaInit =
  kaPatch emptyKaState . fmap Added

-- Incremental update of ka state
-- ------------------------------

kaPatch :: KaState -> KaStateDiff -> KaState
kaPatch st diff =
  let diffDoc = Map.mapWithKey (fmap . parseMarkdown) diff
      diffOutLinks = Map.mapWithKey (fmap . queryLinks) diffDoc
   in st
        { inputDoc =
            applyDiff diffDoc (inputDoc st),
          outLinks =
            applyDiff diffOutLinks (outLinks st),
          graph = (),
          outputDoc =
            -- TODO: Run backlinks plugin to generate this
            mempty,
          outputFiles =
            -- TODO: Run sitegen plugin
            mempty
        }

queryLinks :: FilePath -> FileDoc -> [FilePath]
queryLinks _ _ = [] -- TODO

parseMarkdown :: FilePath -> Text -> FileDoc
parseMarkdown fp s =
  join $ CM.commonmarkWith mempty fp s
