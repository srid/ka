module Ka.Breadcrumb where

import Data.List (elemIndex)
import qualified Data.List.NonEmpty as NE
import Ka.Route
import Reflex.Dom.Core hiding (current)

-- TODO: Capture these invariants in a proper datastructure
data Breadcrumbs a = Breadcrumbs
  { -- | This should be a non empty set, as there can't be duplicates
    _breadcrumbs_trail :: NonEmpty a,
    -- | This should always exist in the above trail.
    _breadcrumbs_current :: a
  }
  deriving (Eq, Show)

init :: a -> Breadcrumbs a
init x =
  Breadcrumbs (one x) x

putCrumb :: Eq a => a -> Breadcrumbs a -> Breadcrumbs a
putCrumb x (Breadcrumbs trail current) =
  if x `elem` toList trail
    then Breadcrumbs trail x
    else case elemIndex current (toList trail) of
      Nothing ->
        error "breadcrumbs error: not possible"
      Just idx ->
        let trail' = x :| (current : reverse (take idx $ toList trail))
         in Breadcrumbs (NE.reverse trail') x

render ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, Prerender js t m) =>
  Dynamic t (Breadcrumbs Route) ->
  m (Event t Route)
render routeHist = do
  divClass "ui mini steps" $ do
    switchHold never <=< dyn $
      ffor routeHist $ \Breadcrumbs {..} -> fmap (leftmost . toList) $
        forM _breadcrumbs_trail $ \rPrev -> do
          elClass "a" (bool "completed step" "active step" $ rPrev == _breadcrumbs_current) $ do
            divClass "content" $ do
              divClass "title" $ do
                routeLink rPrev $ renderRouteText rPrev