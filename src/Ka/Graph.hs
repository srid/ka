module Ka.Graph
  ( Graph,
    empty,
    patch,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Pandoc.Definition (Block)
import Prelude hiding (empty)

type Graph' = AM.AdjacencyMap

-- A graph of notes, with surrounding link context as edge label.
type Graph = Graph' [Block] FilePath

empty :: Graph
empty = AM.empty

patch ::
  ( Ord v,
    Eq e,
    Monoid e
  ) =>
  Map v (Maybe [(e, v)]) ->
  Graph' e v ->
  Graph' e v
patch diff =
  fmap snd . runState $ do
    patchVertex `mapM_` Map.toList diff

patchVertex ::
  (Ord v, Eq e, Monoid e, MonadState (Graph' e v) m) =>
  (v, (Maybe [(e, v)])) ->
  m ()
patchVertex (v, mes) =
  case mes of
    Nothing ->
      modify $ AM.removeVertex v
    Just es -> do
      g <- get
      let removed = AM.postSet v g `Set.difference` (Set.fromList $ snd <$> es)
      forM_ (toList removed) $
        modify . AM.removeEdge v
      modify $
        AM.overlay $
          AM.edges $
            (\(e, v1) -> (e, v, v1)) <$> es
