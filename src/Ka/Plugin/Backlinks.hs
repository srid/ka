module Ka.Plugin.Backlinks where

import Clay (em, pct, (?))
import qualified Clay as C
import Control.Monad.Fix (MonadFix)
import Ka.Graph (Context, Graph, ThingName)
import qualified Ka.Graph as G
import qualified Ka.PandocView as PandocView
import qualified Ka.Plugin.Calendar as Calendar
import Ka.Route (R, Route, renderThingLink)
import Reflex
import Reflex.Dom
import Reflex.Dom.Pandoc (PandocBuilder)
import Text.Pandoc.Definition (Pandoc (Pandoc))

style :: C.Css
style =
  ".backlinks" ? do
    let smallerFont x = C.important $ C.fontSize x
    C.important $ C.backgroundColor "#eee"
    "h2" ? smallerFont (em 1.2)
    "h3" ? smallerFont (pct 90)
    ".context" ? smallerFont (pct 85)
    C.color C.gray
    do
      let linkColorFaded = "#555"
      C.a ? do
        C.important $ do
          C.color linkColorFaded
      C.a C.# C.hover ? do
        C.important $ do
          C.backgroundColor linkColorFaded
          C.color C.white

thingPanel ::
  forall js t m.
  ( Prerender js t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    PandocBuilder t m
  ) =>
  Dynamic t Graph ->
  Dynamic t ThingName ->
  m (Event t (R Route))
thingPanel g thDyn =
  divClass "ui backlinks segment" $ do
    elClass "h2" "header" $ text "Backlinks"
    -- Push dairy entries below, as well in reverse chronological order.
    let ((fmap lefts &&& fmap (reverse . rights)) -> (backlinks, dailyBacklinks)) =
          partitionBacklinks <$> zipDynWith G.preSetWithLabel thDyn g
    e1 <-
      fmap (switch . current . fmap leftmost) $
        simpleList backlinks renderBacklink
    e2 <-
      fmap (switch . current . fmap leftmost) $
        simpleList dailyBacklinks renderBacklink
    pure $ leftmost [e1, e2]
  where
    partitionBacklinks bs =
      fmap (\bl -> if Calendar.includeInSidebar (fst bl) then Left bl else Right bl) bs
    renderBacklink :: Dynamic t (ThingName, [G.Context]) -> m (Event t (R Route))
    renderBacklink ctxDyn = do
      divClass "ui vertical segment" $ do
        evt1 <- elAttr "h3" ("class" =: "header") $ do
          switchHold never <=< dyn $ ffor (fst <$> ctxDyn) renderThingLink
        evt2 <- elClass "ul" "ui list context" $ do
          renderContext $ snd <$> ctxDyn
        pure $ leftmost [evt1, evt2]

renderContext :: (PandocBuilder t f, PostBuild t f, MonadHold t f, MonadFix f, Prerender js t f) => Dynamic t [Context] -> f (Event t (R Route))
renderContext ctxDyn = do
  fmap (switch . current . fmap leftmost) $
    simpleList ctxDyn $ \blkDyn -> do
      el "li" $
        PandocView.render $ ffor blkDyn $ Pandoc mempty . one