module Ka.Breadcrumb where

import Ka.Route
import Reflex.Dom.Core

pushCrumb :: forall a. Eq a => a -> NonEmpty a -> NonEmpty a
pushCrumb x xs =
  case go xs of
    Just ys -> ys
    Nothing -> x :| toList xs
  where
    -- Find x, returning the rest of the given list.
    go :: NonEmpty a -> Maybe (NonEmpty a)
    go = \case
      y :| ys
        | x == y ->
          Just $ x :| ys
      _y :| [] ->
        Nothing
      _ :| (y : ys) ->
        go $ y :| ys

render ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, Prerender js t m) =>
  Dynamic t (NonEmpty Route) ->
  m (Event t Route)
render routeHist = do
  divClass "ui mini steps" $ do
    switchHold never <=< dyn $
      ffor (reverse . headTagged "active step" <$> routeHist) $ \rh -> fmap (leftmost . toList) $
        forM rh $ \(rPrev, mclass) -> do
          elClass "a" (fromMaybe "completed step" mclass) $ do
            divClass "content" $ do
              divClass "title" $ do
                routeLink rPrev $ renderRouteText rPrev
  where
    headTagged :: tag -> NonEmpty a -> [(a, Maybe tag)]
    headTagged t (x :| xs) =
      (x, Just t) : fmap (,Nothing) xs