module Ka.Graph
  ( Graph,
    empty,
    patch,
    patchMap,
    postSetWithLabel,
    preSetWithLabel,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import qualified Data.Map.Strict as Map
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
      -- Remove all edges, then add new ones back in.
      forM_ (toList $ AM.postSet v g) $
        modify . AM.removeEdge v
      modify $
        AM.overlay $
          AM.edges $
            (\(e, v1) -> (e, v, v1)) <$> es

patchMap :: Ord k => Map k (Maybe a) -> Map k a -> Map k a
patchMap diff xs =
  let (toAdd, toDel) = Map.mapEither (maybeToLeft ()) diff
   in Map.union toAdd xs `Map.difference` toDel

postSetWithLabel :: (Ord a, Monoid e) => a -> Graph' e a -> [(a, e)]
postSetWithLabel v g =
  let es = toList $ AM.postSet v g
   in es <&> \v1 ->
        (v1,) $ AM.edgeLabel v v1 g

preSetWithLabel :: (Ord a, Monoid e) => a -> Graph' e a -> [(a, e)]
preSetWithLabel v g =
  let es = toList $ AM.preSet v g
   in es <&> \v0 ->
        (v0,) $ AM.edgeLabel v0 v g