module Ka.Scope where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Reflex (ffor)
import System.FilePath (splitFileName)

-- | The scope of a thing: that is, parent directories. Can be empty if
-- top-level scope.
type ThingScope = [FilePath]

noScope :: ThingScope
noScope = []

-- | Move the parent directories of key to value, retaining only the base name
-- on the key.
--
-- On conflict, take the Just value discarding the Nothing. With two Just
-- values, pick the greater of them (per Map.mapKeysWith semantics); this is
-- irrelevant to our app domain however (so effectively, the behaviour when
-- there are simultaneous modifications to files with the same base name --
-- which produces two Just values -- is undefined).
diffMapScoped :: Map FilePath (Maybe a) -> Map FilePath (Maybe ([FilePath], a))
diffMapScoped m =
  let xs = Map.toList m
   in Map.fromListWith f $
        ffor xs $ \(unScope -> (k, scope), v) ->
          (k, (scope,) <$> v)
  where
    f Nothing Nothing = Nothing
    f Nothing x = x
    f x Nothing = x
    f x _y = x
    -- Convert foo/bar/baz.md into (["foo", "bar"], "baz.md")
    -- TODO: What if `fn` is absolute? As fsnotify might throw in?
    unScope fn =
      let (dir, base) = splitFileName fn
          scope =
            filter (not . null) $
              fmap toString $ T.split (== '/') $ toText dir
       in (base, scope)
