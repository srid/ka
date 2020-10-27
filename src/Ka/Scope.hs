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

showScope :: ThingScope -> Text
showScope =
  T.intercalate "/" . fmap toText

-- | Move the parent directories of key to value, retaining only the base name
-- on the key.
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
      error $
        "Ambiguous file "
          <> toText k
          <> "with multiple scopes: "
          <> showScope scope1
          <> ", "
          <> showScope scope2
    -- Convert foo/bar/baz.md into (["foo", "bar"], "baz.md")
    -- TODO: What if `fn` is absolute? As fsnotify might throw in?
    unScope fn =
      let (dir, base) = splitFileName fn
          scope =
            filter (not . null) $
              fmap toString $ T.split (== '/') $ toText dir
       in (base, scope)
