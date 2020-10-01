module Ka.Database where

import Control.Monad.Writer.Strict
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ka.Diff
import Ka.Graph as G
import Ka.Markdown
import Ka.Plugin
import Text.Pandoc.Definition (Pandoc)

data Db = Db
  { -- | Pandoc AST of plain-text files
    inputDoc :: Map FilePath (V Pandoc),
    -- | Outgoing links (used for building the graph)
    outLinks :: Map FilePath (V (Set FilePath)),
    -- | Graph of notes
    graph :: Graph,
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
      Db mempty mempty G.empty mempty mempty

-- | Change the given `Db` such that *minimal* edits are done to apply the
-- given change.
--
-- This function should run fast given a small number of changes. It most
-- certainly should not be O(n).
changeDb :: Ctx -> Db -> Map FilePath (Changed Text) -> Db
changeDb Ctx {..} (markAllAsUnchanged -> db) txtChanges =
  let cmSpec = commonmarkSpec `foldMap` plugins
      pandocChanges = Map.mapWithKey (fmap . parseMarkdown cmSpec) txtChanges
      outLinksChanges = Map.map (fmap queryLinks) pandocChanges
      inputDoc' = applyChanges pandocChanges $ inputDoc db
      outLinks' = applyChanges outLinksChanges $ outLinks db
      graph' = G.patch (getChange `Map.mapMaybe` outLinks') (graph db)
      outputDoc' =
        flip Map.map inputDoc' $
          mapChanged $
            fmap snd . runState $ do
              forM_ plugins $ \p ->
                modify $ docTransformerWithGraph p graph'
      outputDocChanges = Map.mapMaybe getChange outputDoc'
      extraTouched =
        snd $
          runWriter @(Set FilePath) $ do
            forM_ plugins $ \p ->
              tell $ docTouches p graph' outputDocChanges
      outputDocChanges' = Map.mapMaybe getChange $
        flip Map.mapWithKey outputDoc' $ \k v ->
          if Set.member k extraTouched then markChanged v else v
      outputFiles' =
        flip foldMap (fileGenerator <$> plugins) $ \gen ->
          gen graph' outputDocChanges'
   in db
        { inputDoc = inputDoc',
          outLinks = outLinks',
          graph = graph',
          outputDoc = outputDoc',
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
