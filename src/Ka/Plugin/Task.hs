module Ka.Plugin.Task
  ( runPlugin,
    render,
    Task (..),
    Tasks,
  )
where

import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import Ka.Graph (Graph, ThingName (..))
import qualified Ka.Graph as G
import qualified Ka.PandocView as PandocView
import Ka.Route (Route (..), dynRouteLink, renderRouteText)
import Reflex.Dom.Core hiding (Link)
import Reflex.Dom.Pandoc (PandocBuilder)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc)
import qualified Text.Pandoc.Walk as W

-- TODO:
-- - Enable backlinks (graph connections) automatically.
-- - Hide completed tasks?
-- - Hide tasks from archived notes.
-- - Retain tasklist hierarchy shape.
--  - Perhaps do that in a separate TaskBoard plugin

newtype Task = Task B.Block
  deriving (Eq, Show)

type Tasks = Map ThingName [Task]

runPlugin ::
  forall t m.
  (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m) =>
  Dynamic t Graph ->
  Dynamic t (Map ThingName Pandoc) ->
  (Event t (Map ThingName (Maybe Pandoc))) ->
  m (Event t (Map ThingName (Maybe Tasks)))
runPlugin _graphD _pandocD pandocE = do
  let tasksE = ffor pandocE $ Map.mapMaybe $ traverse (nonEmptyAsJust . extractTasks)
  tasks <- foldDyn G.patchMap mempty tasksE
  pure $
    fforMaybe (attach (current tasks) $ updated tasks) $ \(oldTasks, currTasks) -> do
      guard $ oldTasks /= currTasks
      pure $
        one $
          (ThingName "+Tasks", Just currTasks)

nonEmptyAsJust :: [a] -> Maybe [a]
nonEmptyAsJust =
  fmap toList . nonEmpty

extractTasks :: Pandoc -> [Task]
extractTasks =
  W.query go
  where
    go blk = case blk of
      B.Plain (B.Str "☐" : B.Space : _is) -> [Task blk]
      B.Plain (B.Str "☒" : B.Space : _is) -> [Task blk]
      _ -> []

render ::
  forall js t m.
  (Prerender js t m, PostBuild t m, PandocBuilder t m, MonadHold t m, MonadFix m) =>
  Dynamic t Tasks ->
  m (Event t Route)
render tasks = do
  fmap (switch . current . fmap leftmost) $
    simpleList (Map.toList <$> tasks) $ \xDyn -> do
      r1 <- elClass "h2" "header" $ do
        let r = Route_Node <$> fmap fst xDyn
        dynRouteLink r (constDyn $ "class" =: "route") $ do
          dyn_ $ renderRouteText <$> r
      r2 <- fmap (switch . current . fmap leftmost) $
        simpleList (fmap snd xDyn) $ \taskDyn ->
          PandocView.render $ ffor taskDyn $ \(Task blk) -> B.Pandoc mempty $ one blk
      pure $ leftmost [r1, r2]
