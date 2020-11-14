module Ka.Scope where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Reflex (ffor)
import System.FilePath (splitFileName)

-- TODO: Is the scope data-type perfect?
-- Consider the case of multiple notebooks passed as arguments to CLI
-- Duplicate daily notes: we should allow them! and yet resolve it correctly.

-- | The scope of a thing: that is, parent directories. Can be empty if
-- top-level scope.
type ThingScope = [FilePath]

-- A thing without scope is said to exist at the root directory.
noScope :: ThingScope
noScope = []

rootPath :: FilePath
rootPath = "/"

splitScope :: ThingScope -> (Maybe ThingScope, FilePath)
splitScope = first (fmap reverse) . go . reverse
  where
    go [] = (Nothing, rootPath)
    go (x : xs) = (Just xs, x)

isSubScope :: ThingScope -> ThingScope -> Bool
isSubScope child parent =
  if null parent
    then length child == 1
    else length child == length parent + 1 && parent `isPrefixOf` child

showScope :: ThingScope -> Text
showScope [] =
  toText rootPath
showScope s =
  T.intercalate "/" $ fmap toText s

-- | Move the parent directories of map key to map value, retaining only the
-- base filename on the key.
--
-- On conflict, this function errors out (when there are simultaneous
-- modifications to files with the same base name).
diffMapScoped :: HasCallStack => Map FilePath (Maybe a) -> Map FilePath (Maybe (ThingScope, a))
diffMapScoped (Map.toList -> xs) =
  Map.fromListWithKey f $
    ffor xs $ \(unScope -> (k, scope), v) ->
      (k, (scope,) <$> v)
  where
    f _k Nothing Nothing = Nothing
    f _k Nothing x = x
    f _k x Nothing = x
    f k (Just (scope1, _)) (Just (scope2, _)) =
      -- TODO: Handle this error gracefully.
      error $
        "Ambiguous file "
          <> toText k
          <> "with multiple scopes: "
          <> showScope scope1
          <> ", "
          <> showScope scope2
    -- Convert foo/bar/baz.md into (["foo", "bar"], "baz.md")
    unScope fn =
      let (dir, base) = splitFileName fn
          scope' =
            filter (not . null) $
              fmap toString $ T.split (== '/') $ toText dir
          scope =
            -- because splitFileName returns a dot
            if scope' == ["."] then [] else scope'
       in (base, scope)
