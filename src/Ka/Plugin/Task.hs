module Ka.Plugin.Task
  ( runPlugin,
    render,
    style,
    Task (..),
    Tasks,
  )
where

import Clay ((?))
import qualified Clay as C
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import Ka.Graph (Graph, ThingName (..))
import qualified Ka.Graph as G
import qualified Ka.PandocView as PandocView
import Ka.Route (Route (..), dynRouteLink, renderRouteText)
import Ka.Scope (ThingScope, noScope)
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

data Task = Task
  { _task_block :: B.Block,
    _task_checked :: Bool
  }
  deriving (Eq, Show)

type Tasks = Map ThingName [Task]

runPlugin ::
  forall t m.
  (Reflex t, MonadHold t m, MonadFix m, DomBuilder t m) =>
  Dynamic t Graph ->
  Dynamic t (Map ThingName (ThingScope, Pandoc)) ->
  (Event t (Map ThingName (Maybe (ThingScope, Pandoc)))) ->
  m (Event t (Map ThingName (Maybe (ThingScope, Tasks))))
runPlugin _graphD _pandocD pandocE = do
  let tasksWithScopeE =
        -- The outter maybe is Nothing if the thing has no tasks. We join it
        -- with inner maybe to treat it as "deleted" (because in effect the
        -- tasks in this otherwise existing note may have just been deleted),
        -- and we want that "deletion" to propagate to the foldDyn below for
        -- correct update.
        fmap (fmap join) $
          ffor pandocE $
            Map.map $
              traverse $
                traverse $ \(extractTasks -> ts) -> do
                  ts' <- nonEmptyAsJust ts
                  -- Ignore notes without any unchecked tasks
                  guard $ any (not . _task_checked) ts'
                  pure ts'
      tasksE =
        fmap (fmap snd) <$> tasksWithScopeE
  -- FIXME: When a note gets all its task removed, this does't patch `tasks` to
  -- remove it. We should pass the "informatoin" that certain notes modified in
  -- `pandocE` have no tasks
  tasks <- foldDyn G.patchMap mempty tasksE
  pure $
    fforMaybe (attach (current tasks) $ updated tasks) $ \(oldTasks, currTasks) -> do
      guard $ oldTasks /= currTasks
      pure $
        one $
          (ThingName "+Tasks", Just (noScope, currTasks))

nonEmptyAsJust :: [a] -> Maybe [a]
nonEmptyAsJust =
  fmap toList . nonEmpty

extractTasks :: Pandoc -> [Task]
extractTasks =
  W.query go
  where
    go blk = case blk of
      B.Plain (B.Str "☐" : B.Space : _is) -> [Task blk False]
      B.Plain (B.Str "☒" : B.Space : _is) -> [Task blk True]
      _ -> []

render ::
  forall js t m.
  (Prerender js t m, PostBuild t m, PandocBuilder t m, MonadHold t m, MonadFix m) =>
  Dynamic t Tasks ->
  m (Event t Route)
render tasks = divClass "tasks" $ do
  fmap (switch . current . fmap leftmost) $
    simpleList (Map.toList <$> tasks) $ \xDyn -> do
      r1 <- elClass "h2" "header" $ do
        let r = Route_Node <$> fmap fst xDyn
        dynRouteLink r (constDyn $ "class" =: "route") $ do
          dyn_ $ renderRouteText <$> r
      r2 <- fmap (switch . current . fmap leftmost) $
        simpleList (fmap snd xDyn) $ \taskDyn -> do
          elDynClass "span" (bool "unchecked task" "checked task" <$> _task_checked <$> taskDyn) $
            PandocView.render $
              ffor (_task_block <$> taskDyn) $
                B.Pandoc mempty . one
      pure $ leftmost [r1, r2]

style :: C.Css
style =
  ".tasks" ? do
    ".checked.task" ? do
      C.textDecoration C.lineThrough
      C.color C.gray
