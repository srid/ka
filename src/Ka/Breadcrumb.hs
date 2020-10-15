module Ka.Breadcrumb where

import Control.Monad.Fix (MonadFix)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Ka.Graph (ThingName)
import Ka.Route (Route (..), renderRouteText, routeLinkWithAttr)
import Reflex.Dom.Core

-- | A non empty list with cursor
data Breadcrumbs a = Breadcrumbs
  { _breadcrumbs_before :: [a],
    _breadcrumbs_current :: a,
    _breadcrumbs_after :: [a]
  }
  deriving (Eq, Show)

instance Foldable Breadcrumbs where
  foldMap f (Breadcrumbs before curr after) =
    foldMap f before <> f curr <> foldMap f after

init :: a -> Breadcrumbs a
init x =
  Breadcrumbs [] x []

-- | Put a new crumb, such that it behaves like the stacked navigation of
-- https://notes.andymatuschak.org/
putCrumb :: Eq a => a -> Breadcrumbs a -> Breadcrumbs a
putCrumb x bc@(Breadcrumbs before curr _) =
  case break (== x) (toList bc) of
    (_, []) ->
      Breadcrumbs (before <> [curr]) x []
    (before', _x : after') ->
      Breadcrumbs before' x after'

render ::
  forall t m js.
  ( DomBuilder t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    Prerender js t m,
    MonadIO (Performable m),
    MonadIO m,
    PerformEvent t m,
    TriggerEvent t m
  ) =>
  Dynamic t [ThingName] ->
  Dynamic t (Breadcrumbs Route) ->
  m (Event t Route)
render ths routeHist = do
  divClass "ui right floated small fluid inverted vertical menu" $ do
    evt1 <- fmap (switch . current . fmap leftmost) $
      simpleList (toList <$> routeHist) $ \rPrevD -> switchHold never <=< dyn $
        ffor (zipDyn rPrevD $ _breadcrumbs_current <$> routeHist) $ \(rPrev, rCurr) -> do
          let itemClass = bool "item" "active purple item" $ rPrev == rCurr
          routeLinkWithAttr rPrev (constDyn $ "class" =: itemClass) $ do
            if (rPrev == Route_Main)
              then divClass "content" $ do
                elClass "i" "home icon" blank
                renderClock
              else renderRouteText rPrev
    -- TODO: Move to Search.hs and implement
    void $
      divClass "item" $ do
        divClass "ui input" $ do
          inputElement $
            def
              & initialAttributes .~ ("placeholder" =: "Press / to search")
    evt2 <- fmap (switch . current . fmap leftmost) $
      simpleList ths $ \thD -> do
        switchHold never <=< dyn $
          ffor thD $ \th -> do
            let r = Route_Node th
            routeLinkWithAttr r (constDyn $ "class" =: "gray active item") $ do
              renderRouteText r
    pure $ leftmost [evt1, evt2]

renderClock ::
  ( MonadIO m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    PerformEvent t m,
    TriggerEvent t m,
    DomBuilder t m
  ) =>
  m ()
renderClock = do
  now <- liftIO getCurrentTime
  let secs = 1
  tick <- clockLossy secs now
  dyn_ $ renderTick <$> tick
  where
    renderTick TickInfo {..} =
      el "time" $ text $ toText $ formatTime defaultTimeLocale "%T" _tickInfo_lastUTC
