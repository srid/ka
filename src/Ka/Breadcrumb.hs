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

init :: a -> Breadcrumbs a
init x =
  Breadcrumbs [] x []

allCrumbs :: Breadcrumbs a -> [a]
allCrumbs (Breadcrumbs before current after) =
  before <> [current] <> after

-- | Put a new crumb, such that it behaves like the stacked navigation of
-- https://notes.andymatuschak.org/
putCrumb :: Eq a => a -> Breadcrumbs a -> Breadcrumbs a
putCrumb x bc@(Breadcrumbs before current _) =
  case break (== x) (allCrumbs bc) of
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
        fmap (leftmost . toList) $
          forM (allCrumbs bc) $ \rPrev -> do
            elClass "a" (bool "completed step" "active step" $ rPrev == _breadcrumbs_current) $ do
              divClass "content" $ do
                divClass "title" $ do
                  routeLink rPrev $ renderRouteText rPrev