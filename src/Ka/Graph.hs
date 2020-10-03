module Ka.Graph
  ( Graph,
    empty,
    patch,
  )
where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ka.Watch (Status (..))
import Prelude hiding (empty)

type Graph = AM.AdjacencyMap FilePath

empty :: Graph
empty = AM.empty

patch :: Map FilePath ([FilePath], Status) -> Graph -> Graph
patch diff =
  fmap snd . runState $ do
    patchVertex `mapM_` Map.toList diff

patchVertex ::
  MonadState Graph m =>
  (FilePath, ([FilePath], Status)) ->
  m ()
patchVertex (v, (es, st)) =
  case st of
    Deleted ->
      modify $ AM.removeVertex v
    Untracked ->
      modify $ AM.overlay $ AM.star v es
    Dirty -> do
      g <- get
      let removed = AM.postSet v g `Set.difference` (Set.fromList es)
      forM_ (toList removed) $
        modify . AM.removeEdge v
      modify $ AM.overlay $ AM.star v es