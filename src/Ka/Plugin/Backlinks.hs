module Ka.Plugin.Backlinks where

import Clay (em, pct, (?))
import qualified Clay as C
import Control.Monad.Fix (MonadFix)
import Ka.Graph (Graph, ThingName)
import qualified Ka.Graph as G
import qualified Ka.PandocView as PandocView
import Ka.Route (Route, renderThingLink)
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
  ( Prerender js t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    PandocBuilder t m
  ) =>
  Dynamic t Graph ->
  Dynamic t ThingName ->
  m (Event t Route)
thingPanel g thDyn =
  divClass "ui backlinks segment" $ do
    let backlinks = zipDynWith G.preSetWithLabel thDyn g
    elClass "h2" "header" $ text "Backlinks"
    fmap (switch . current . fmap leftmost) $
      simpleList backlinks $ \ctxDyn -> do
        divClass "ui vertical segment" $ do
          evt1 <- elAttr "h3" ("class" =: "header") $ do
            switchHold never <=< dyn $ ffor (fst <$> ctxDyn) renderThingLink
          evt2 <- elClass "ul" "ui list context" $ do
            fmap (switch . current . fmap leftmost) $
              simpleList (snd <$> ctxDyn) $ \blkDyn -> do
                el "li" $
                  PandocView.render $ ffor blkDyn $ Pandoc mempty . one
          pure $ leftmost [evt1, evt2]
