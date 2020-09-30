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
  compose $
    (one . uncurry patchVertex)
      `concatMap` Map.toList diff

patchVertex :: FilePath -> Changed (Set FilePath) -> Graph -> Graph
patchVertex v esC g =
  flip compose g $ case esC of
    Removed ->
      one $ AM.removeVertex v
    Added es ->
      one $ AM.overlay $ AM.star v (Set.toList es)
    Modified es ->
      let removed = AM.postSet v g `Set.difference` es
       in mconcat
            [ AM.removeEdge v <$> toList removed,
              one $ AM.overlay $ AM.star v (Set.toList es)
            ]

compose :: [a -> a] -> a -> a
compose = foldr (.) id