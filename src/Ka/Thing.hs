module Ka.Thing
  ( Thing (..),
    render,
    style,
  )
where

import Clay ((?))
import qualified Clay as C
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare (GEq (geq))
import Data.Time.Calendar (Day)
import Data.Type.Equality
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
  Thing_Calendar :: Thing (Set Day)

render ::
  ( Prerender js t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    PandocBuilder t m
  ) =>
  Dynamic t Graph ->
  Dynamic t (ThingName, DSum Thing Identity) ->
  m (Event t Route)
render g thVal = do
  divClass "ui basic attached segments thing" $ do
    thValF <- factorDyn $ snd <$> thVal
    r1 <- divClass "ui attached basic segment" $ do
      elClass "h1" "header" $ dynText $ unThingName . fst <$> thVal
      switchHold never <=< dyn $
        ffor thValF $ \case
          Thing_Pandoc :=> (fmap runIdentity . getCompose -> doc) ->
            PandocView.render doc
          Thing_Calendar :=> (fmap runIdentity . getCompose -> days) ->
            Calendar.render days
    -- TODO: Have to figure our UI order of plugins
    r3 <- Calendar.thingPanel g $ fst <$> thVal
    r2 <- Backlinks.thingPanel g $ fst <$> thVal
    pure $ leftmost [r1, r2, r3]

style :: C.Css
style = do
  ".thing" ? do
    PandocView.style
    Backlinks.style

-- This breaks ghcide :-/
-- https://github.com/haskell/haskell-language-server/pull/463
{-
import Data.GADT.Compare.TH (deriveGEq)
fmap concat $
  sequence
    [ deriveGEq ''Thing
    ]
-}

-- Why derive manually? See above.
instance GEq Thing where
  geq Thing_Pandoc Thing_Pandoc =
    pure Refl
  geq Thing_Calendar Thing_Calendar =
    pure Refl
  geq _ _ = Nothing
