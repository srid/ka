-- | A diff is a set (map) of `Change`s
module Ka.Diff where

import qualified Data.Map.Strict as Map

-- | The kind of change made to some value
data Change a
  = Added a
  | Modified a
  | Removed
  deriving stock (Eq, Show)
  deriving stock (Functor)

changedVal :: Change a -> Maybe a
changedVal = \case
  Added x -> Just x
  Modified x -> Just x
  Removed -> Nothing

-- | Value that may have been changed
data V a
  = -- | The value has not been modified
    VSame a
  | -- | The value was modified, added or deleted
    VChanged (Change a)
  deriving (Eq, Show)

-- | Mark as value as unchanged (same), unless it was removed (then return Nothing)
markSame :: V a -> Maybe (V a)
markSame = \case
  VChanged ch -> VSame <$> changedVal ch
  x@(VSame _) -> Just x

applyDiff :: Ord k => Map k (Change a) -> Map k (V a) -> Map k (V a)
applyDiff diff =
  Map.union (fmap VChanged diff)
