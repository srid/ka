module Ka.Graph
  ( Graph,
    empty,
    patch,
  )
where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ka.Diff
import Prelude hiding (empty)

-- TODO:
-- WikLink: use .md.
-- query for links to "*.md"
-- Render: change .md to .html

type Graph = AM.AdjacencyMap FilePath

empty :: Graph
empty = AM.empty

patch :: Map FilePath (Changed (Set FilePath)) -> Graph -> Graph
patch diff =
  fmap snd . runState $ do
    uncurry patchVertex `mapM_` Map.toList diff

patchVertex :: MonadState Graph m => FilePath -> Changed (Set FilePath) -> m ()
patchVertex v = \case
  Removed ->
    modify $ AM.removeVertex v
  Added es ->
    modify $ AM.overlay $ AM.star v (Set.toList es)
  Modified es -> do
    g <- get
    let removed = AM.postSet v g `Set.difference` es
    forM_ (toList removed) $
      modify . AM.removeEdge v
    modify $ AM.overlay $ AM.star v (Set.toList es)