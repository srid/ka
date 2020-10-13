module Ka.Breadcrumb where

import Ka.Route
import Reflex.Dom.Core hiding (current)

-- | A non empty list with cursor
data Breadcrumbs a = Breadcrumbs
  { _breadcrumbs_before :: [a],
    _breadcrumbs_current :: a,
    _breadcrumbs_after :: [a]
  }
  deriving (Eq, Show)

instance Foldable Breadcrumbs where
  foldMap f (Breadcrumbs before current after) =
    foldMap f before <> f current <> foldMap f after

init :: a -> Breadcrumbs a
init x =
  Breadcrumbs [] x []

-- | Put a new crumb, such that it behaves like the stacked navigation of
-- https://notes.andymatuschak.org/
putCrumb :: Eq a => a -> Breadcrumbs a -> Breadcrumbs a
putCrumb x bc@(Breadcrumbs before current _) =
  case break (== x) (toList bc) of
    (_, []) ->
      Breadcrumbs (before <> [current]) x []
    (before', _x : after') ->
      Breadcrumbs before' x after'

render ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, Prerender js t m) =>
  Dynamic t (Breadcrumbs Route) ->
  m (Event t Route)
render routeHist = do
  divClass "ui mini steps" $ do
    switchHold never <=< dyn $
      ffor routeHist $ \bc@Breadcrumbs {..} -> do
        fmap leftmost $
          forM (toList bc) $ \rPrev -> do
            elClass "a" (bool "completed step" "active step" $ rPrev == _breadcrumbs_current) $ do
              divClass "content" $ do
                divClass "title" $ do
                  routeLink rPrev $ renderRouteText rPrev