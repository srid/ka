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
      kaApp "./doc"
      pure never

kaApp :: MonadHeadlessApp t m => FilePath -> m ()
kaApp inputDir = do
  pb <- getPostBuild
  changes <- watchDir defaultConfig (inputDir <$ pb) (const True)
  fs0 :: Map FilePath Text <- fmap Map.fromList $ do
    liftIO $ do
      files <- getDirectoryFiles inputDir ["*.md"]
      forM files $ \fp ->
        (fp,) <$> readFileText (inputDir </> fp)
  fsE :: Event t KaStateDiff <-
    performEvent $
      ffor changes $
        -- TODO: group events
        fmap one . \case
          FSN.Added fp _ _ -> (fp,) . Just <$> readFileText fp
          FSN.Modified fp _ _ -> (fp,) . Just <$> readFileText fp
          FSN.Removed fp _ _ -> pure (fp, Nothing)
          FSN.Unknown fp _ _ -> pure (fp, Nothing) -- FIXME: ?
  rec kaWithDiff :: Dynamic t (KaState, KaStateDiff) <-
        holdDyn (kaInit fs0, mempty) $
          ffor (attach (current kaWithDiff) fsE) $ \((things, _lastChanged), changed) ->
            (kaPatch things changed, changed)
  void $
    networkView $
      ffor kaWithDiff $ \(st, diff) -> do
        liftIO $ print st
        liftIO $ print diff
        liftIO $ putStrLn "===\n"

-- ka types
-- --------

type FileContent = Text

type FileDoc = Either CM.ParseError (CP.Cm () B.Blocks)

data KaState = KaState
  { -- | Pandoc AST of plain-text files
    inputDoc :: Map FilePath FileDoc,
    -- | Graph of notes
    graph :: (),
    -- | Post-transformed Pandoc AST
    outputDoc :: Map FilePath FileDoc,
    -- | Pages to generate
    outputFiles :: Map FilePath ByteString
  }
  deriving (Show)

type KaStateDiff = Map FilePath (Maybe FileContent)

emptyKaState :: KaState
emptyKaState =
  KaState mempty () mempty mempty

kaInit :: Map FilePath FileContent -> KaState
kaInit =
  kaPatch emptyKaState . fmap Just

-- Incremental update of ka state
-- ------------------------------

kaPatch :: KaState -> KaStateDiff -> KaState
kaPatch st diff =
  let (toAdd, toDel) = Map.mapEither (maybeToLeft ()) diff
   in st
        { inputDoc =
            Map.union (flip Map.mapWithKey toAdd parseMarkdown) (inputDoc st)
              `Map.difference` toDel
        }

parseMarkdown :: FilePath -> Text -> FileDoc
parseMarkdown fp s =
  join $ CM.commonmarkWith mempty fp s
