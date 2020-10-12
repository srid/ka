module Ka.Breadcrumb where

import Data.List (elemIndex)
import qualified Ka.Graph as G
import Ka.Route
import Reflex.Dom.Core

pushCrumb :: Eq a => a -> [a] -> [a]
pushCrumb x xs =
  case elemIndex x xs of
    Nothing -> x : xs
    Just idx -> drop idx xs

render ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, Prerender js t m) =>
  Dynamic t [Route] ->
  m (Event t Route)
render routeHist = do
  divClass "ui mini steps" $ do
    switchHold never <=< dyn $
      ffor (reverse <$> routeHist) $ \rh -> fmap leftmost $
        forM rh $ \rPrev -> do
          elClass "a" "step" $ do
            divClass "content" $ do
              divClass "title" $ do
                -- TODO: clicking on this should discard later items in stack
                routeLink rPrev $ case rPrev of
                  Route_Main -> text "Main"
                  Route_Node th -> text $ G.unThing th