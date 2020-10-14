module Ka.Thing
  ( Thing (..),
    render,
    style,
  )
where

import Clay (em, pct, (?))
import qualified Clay as C
import Data.Dependent.Sum (DSum (..))
import Ka.Graph (Graph, ThingName (..))
import qualified Ka.Graph as G
import qualified Ka.PandocView as PandocView
import qualified Ka.Plugin.Calendar as Calendar
import Ka.Route (Route, renderThingLink)
import Reflex
import Reflex.Dom
import Reflex.Dom.Pandoc (PandocBuilder)
import Text.Pandoc.Definition (Pandoc (..))

-- | All kinds of things managed by plugins.
data Thing a where
  -- | The most common type; simply corresponds to Markdown files written by the
  -- user.
  Thing_Pandoc :: Thing Pandoc
  -- | The type used by the Calendar plugin; holds the list of daily notes.
  Thing_Calendar :: Thing (Set ThingName)

render ::
  ( Prerender js t m,
    PostBuild t m,
    PandocBuilder t m
  ) =>
  Graph ->
  ThingName ->
  DSum Thing Identity ->
  m (Event t Route)
render g th v = do
  divClass "thing" $ do
    elClass "h1" "ui header" $ text $ unThingName th
    r1 <- case v of
      Thing_Pandoc :=> Identity doc ->
        PandocView.render doc
      Thing_Calendar :=> Identity days ->
        Calendar.render days
    r2 <- divClass "ui backlinks segment" $ do
      let backlinks = G.preSetWithLabel th g
      backlinksWidget backlinks
    pure $ leftmost [r1, r2]

style :: C.Css
style = do
  ".thing" ? do
    PandocView.style
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

backlinksWidget ::
  (Prerender js t m, PostBuild t m, PandocBuilder t m) =>
  [(ThingName, [G.Context])] ->
  m (Event t Route)
backlinksWidget xs = do
  elClass "h2" "header" $ text "Backlinks"
  fmap leftmost $
    forM xs $ \(x, blks) -> do
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
