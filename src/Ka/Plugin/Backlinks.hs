module Ka.Plugin.Backlinks where

import Clay (em, pct, (?))
import qualified Clay as C
import Ka.Graph (Graph, ThingName)
import qualified Ka.Graph as G
import qualified Ka.PandocView as PandocView
import Ka.Route (Route, renderThingLink)
import Reflex
import Reflex.Dom
import Reflex.Dom.Pandoc (PandocBuilder)
import Text.Pandoc.Definition (Pandoc (Pandoc))

style :: C.Css
style = do
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

backlinksWidget ::
  (Prerender js t m, PostBuild t m, PandocBuilder t m) =>
  Graph ->
  ThingName ->
  m (Event t Route)
backlinksWidget g th = do
  let backlinks = G.preSetWithLabel th g
  elClass "h2" "header" $ text "Backlinks"
  fmap leftmost $
    forM backlinks $ \(x, blks) -> do
      divClass "ui vertical segment" $ do
        evt1 <- elAttr "h3" ("class" =: "header") $ do
          renderThingLink x
        evt2 <- elClass "ul" "ui list context" $ do
          fmap leftmost $
            forM blks $ \blk -> do
              let blkThing = Pandoc mempty (one blk)
              el "li" $
                PandocView.render blkThing
        pure $ leftmost [evt1, evt2]
