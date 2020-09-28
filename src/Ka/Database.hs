module Ka.Database where

import qualified Data.Map.Strict as Map
import Ka.Diff
import Ka.Markdown
import Ka.Plugin
import Text.Pandoc.Definition (Pandoc)

data Db = Db
  { -- | Pandoc AST of plain-text files
    inputDoc :: Map FilePath (V Pandoc),
    -- | Outgoing links (used for building the graph)
    outLinks :: Map FilePath (V [FilePath]),
    -- | Graph of notes
    graph :: (),
    -- | Post-transformed Pandoc AST
    outputDoc :: Map FilePath (V Pandoc),
    -- | Pages to generate
    outputFiles :: Map FilePath (Changed ByteString)
  }
  deriving (Show)

data Ctx = Ctx
  { plugins :: [Plugin]
  }

initDb :: Ctx -> Map FilePath Text -> Db
initDb ctx =
  patchDb ctx emptyDb . fmap Added
  where
    emptyDb =
      Db mempty mempty () mempty mempty

resetDb :: Db -> Db
resetDb st =
  st
    { inputDoc = Map.mapMaybe id $ fmap markUnchanged $ inputDoc st,
      outLinks = Map.mapMaybe id $ fmap markUnchanged $ outLinks st,
      outputDoc = Map.mapMaybe id $ fmap markUnchanged $ outputDoc st,
      outputFiles = mempty
    }

patchDb :: Ctx -> Db -> Map FilePath (Changed Text) -> Db
patchDb Ctx {..} (resetDb -> st) diff =
  let cmSpec = commonmarkSpec `foldMap` plugins
      diffDoc = Map.mapWithKey (fmap . parseMarkdown cmSpec) diff
      diffOutLinks = Map.mapWithKey (fmap . queryLinks) diffDoc
      inputDoc' = applyChanges diffDoc $ inputDoc st
      outLinks' = applyChanges diffOutLinks $ outLinks st
      outputFiles' :: Map FilePath (Changed ByteString) =
        flip foldMap (fileGenerator <$> plugins) $ \gen ->
          gen inputDoc'
   in st
        { inputDoc = inputDoc',
          outLinks = outLinks',
          graph = (),
          outputDoc = inputDoc',
          outputFiles = outputFiles'
        }
