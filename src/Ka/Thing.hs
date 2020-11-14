{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Ka.Thing
  ( ThingVal (..),
    Thing (..),
    render,
    style,
  )
where

import Clay ((?))
import qualified Clay as C
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare.TH (deriveGEq)
import Data.Time.Calendar (Day)
import Ka.Graph (Graph, ThingName (..))
import qualified Ka.PandocView as PandocView
import qualified Ka.Plugin.Backlinks as Backlinks
import qualified Ka.Plugin.Calendar as Calendar
import qualified Ka.Plugin.Task as Task
import qualified Ka.Plugin.Telescope as Telescope
import Ka.Route (R, Route)
import Ka.Scope (ThingScope)
import Reflex
import Reflex.Dom
import Reflex.Dom.Pandoc (PandocBuilder)
import Text.Pandoc.Definition (Pandoc (..))

data Thing = Thing
  { _thing_scope :: ThingScope,
    _thing_val :: DSum ThingVal Identity
  }

-- | All kinds of things managed by plugins.
data ThingVal a where
  -- | The most common type; simply corresponds to Markdown files written by the
  -- user.
  ThingVal_Pandoc :: ThingVal Pandoc
  -- | The type used by the Calendar plugin; holds the list of daily notes.
  ThingVal_Calendar :: ThingVal (Set Day)
  ThingVal_Tasks :: ThingVal Task.Tasks

render ::
  ( Prerender js t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    PandocBuilder t m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m)
  ) =>
  Dynamic t Graph ->
  Dynamic t (Map ThingName Thing) ->
  Dynamic t (ThingName, Thing) ->
  m (Event t (R Route))
render g doc thVal = do
  thValF <- factorDyn $ _thing_val . snd <$> thVal
  r1 <- divClass "ui attached basic segment" $ do
    elClass "h1" "header" $ dynText $ unThingName . fst <$> thVal
    switchHold never <=< dyn $
      ffor thValF $ \case
        ThingVal_Pandoc :=> (fmap runIdentity . getCompose -> docDyn) ->
          PandocView.render docDyn
        ThingVal_Calendar :=> (fmap runIdentity . getCompose -> days) ->
          Calendar.render g days
        ThingVal_Tasks :=> (fmap runIdentity . getCompose -> x) ->
          Task.render x
  -- TODO: Have to figure our UI order of plugins
  r3 <- Calendar.thingPanel g $ fst <$> thVal
  r2 <- Backlinks.thingPanel g $ fst <$> thVal
  r4 <- Telescope.thingPanel g (fmap _thing_scope <$> doc) $ second _thing_scope <$> thVal
  pure $ leftmost [r1, r2, r3, r4]

style :: C.Css
style = do
  ".thing" ? do
    PandocView.style
    Backlinks.style
    Telescope.style
    Task.style

$(deriveGEq ''ThingVal)