module Ka.Thing
  ( Thing (..),
    render,
    style,
  )
where

import Clay ((?))
import qualified Clay as C
import Data.Dependent.Sum (DSum (..))
import Ka.Graph (Graph, ThingName (..))
import qualified Ka.PandocView as PandocView
import qualified Ka.Plugin.Backlinks as Backlinks
import qualified Ka.Plugin.Calendar as Calendar
import Ka.Route (Route)
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
    -- TODO: Have to figure our UI order of plugins
    r3 <- Calendar.thingPanel g th
    r2 <- Backlinks.thingPanel g th
    pure $ leftmost [r1, r2, r3]

style :: C.Css
style = do
  ".thing" ? do
    PandocView.style
    Backlinks.style
