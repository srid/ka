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
    outputFiles :: Map FilePath (Changed (IO ByteString))
  }

-- | Unlike Db, Ctx doesn't change, and is fixed and required for Db updates.
data Ctx = Ctx
  { plugins :: [Plugin]
  }

initDb :: Ctx -> Map FilePath Text -> Db
initDb ctx (fmap Added -> changes) =
  changeDb ctx emptyDb changes
  where
    emptyDb =
      Db mempty mempty () mempty mempty

-- | Change the given `Db` such that *minimal* edits are done to apply the
-- given change.
--
-- This function should run fast given a small number of changes. It most
-- certainly should not be O(n).
changeDb :: Ctx -> Db -> Map FilePath (Changed Text) -> Db
changeDb Ctx {..} (markAllAsUnchanged -> db) txtChanges =
  let cmSpec = commonmarkSpec `foldMap` plugins
      pandocChanges = Map.mapWithKey (fmap . parseMarkdown cmSpec) txtChanges
      outLinksChanges = Map.mapWithKey (fmap . queryLinks) pandocChanges
      inputDoc' = applyChanges pandocChanges $ inputDoc db
      outLinks' = applyChanges outLinksChanges $ outLinks db
      outputFiles' =
        flip foldMap (fileGenerator <$> plugins) $ \gen ->
          gen inputDoc'
   in db
        { inputDoc = inputDoc',
          outLinks = outLinks',
          graph = (),
          outputDoc = inputDoc',
          outputFiles = outputFiles'
        }

markAllAsUnchanged :: Db -> Db
markAllAsUnchanged st =
  st
    { inputDoc = Map.mapMaybe id $ fmap markUnchanged $ inputDoc st,
      outLinks = Map.mapMaybe id $ fmap markUnchanged $ outLinks st,
      outputDoc = Map.mapMaybe id $ fmap markUnchanged $ outputDoc st,
      outputFiles = mempty
    }
