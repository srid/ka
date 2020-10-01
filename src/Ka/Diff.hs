module Ka.Diff where

import qualified Data.Map.Strict as Map

-- | A value that was changed in some way.
data Changed a
  = Added a
  | Modified a
  | Removed
  deriving stock (Eq, Show)
  deriving stock (Functor)

changedTo :: Changed a -> Maybe a
changedTo = \case
  Added x -> Just x
  Modified x -> Just x
  Removed -> Nothing

-- | A value that *may* have been changed
data V a
  = -- | The value has not been modified
    VSame a
  | -- | The value was modified, added or deleted
    VChanged (Changed a)
  deriving stock (Eq, Show)

mapChanged :: (a -> a) -> V a -> V a
mapChanged f = \case
  VSame x -> VSame x
  VChanged ch -> VChanged $ fmap f ch

getChange :: V a -> Maybe (Changed a)
getChange = \case
  VSame _ -> Nothing
  VChanged ch -> Just ch

markChanged :: V a -> V a
markChanged = \case
  VSame x -> VChanged (Modified x)
  x -> x

-- | Mark as value as unchanged (same), unless it was removed (then return Nothing)
markUnchanged :: V a -> Maybe (V a)
markUnchanged = \case
  VChanged ch -> VSame <$> changedTo ch
  x@(VSame _) -> Just x

-- | Apply changes using Map as the structure.
applyChanges :: Ord k => Map k (Changed a) -> Map k (V a) -> Map k (V a)
applyChanges diff =
  Map.union (fmap VChanged diff)
