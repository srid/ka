module Ka.Plugin.Telescope
  ( style,
    thingPanel,
  )
where

import qualified Algebra.Graph.AdjacencyMap.Algorithm as AM
import qualified Algebra.Graph.Labelled.AdjacencyMap as AM
import Clay ((?))
import qualified Clay as C
import Control.Monad.Fix (MonadFix)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ka.Graph (Graph, ThingName (unThingName))
import qualified Ka.Plugin.Calendar as Calendar
import Ka.Route (Route, renderThingLink)
import Ka.Scope (ThingScope)
import Reflex
import Reflex.Dom
import Reflex.Dom.Pandoc (PandocBuilder)

style :: C.Css
style =
  ".telescope" ? do
    C.important $ C.backgroundColor "#eee"
    C.color C.gray

thingPanel ::
  forall js t m.
  ( Prerender js t m,
    PostBuild t m,
    MonadHold t m,
    MonadFix m,
    PandocBuilder t m
  ) =>
  Dynamic t Graph ->
  Dynamic t (Map ThingName ThingScope) ->
  Dynamic t (ThingName, ThingScope) ->
  m (Event t Route)
thingPanel g' scopeDyn thWithScopeDyn = do
  let g = ffor3 g' thWithScopeDyn scopeDyn $ \graph (_name, scope) scopes ->
        AM.induce (includeThing $ Map.filter (== scope) scopes) graph
      thDyn = fst <$> thWithScopeDyn
  -- TODO: ^^ Might have to narrow the graph to contain
  divClass "ui telescope segment" $ do
    elClass "h2" "header" $ do
      text "Telescope ["
      dynText $ T.intercalate "/" . fmap toText . snd <$> thWithScopeDyn
      text "]"
    el "p" $ do
      text "Notes reachable from "
      el "em" $ dynText $ unThingName <$> thDyn
      text ", in increasing order of distance from it."
      el "strong" $ text " Please note: "
      text "the Telescope plugin is still in development."
    let layers = ffor2 g thDyn $ \graph th ->
          let graphUndirected = AM.overlay graph $ AM.transpose graph
           in -- Discard the first item of the result which would be `[th]`
              maybe [] tail $
                nonEmpty $
                  AM.bfs [th] (AM.skeleton graphUndirected)
    -- TODO: Tree layout, with interactive path visuals?
    fmap (switch . current . fmap leftmost) $
      simpleList (zip [1 :: Int ..] <$> layers) $ \(fmap fst &&& fmap snd -> (idxDyn, layerDyn)) -> do
        el "h3" $ dynText $ show <$> idxDyn
        fmap (switch . current . fmap leftmost) $
          simpleList layerDyn $ \thsDyn -> do
            divClass "ui label" $ do
              switchHold never <=< dyn $ renderThingLink <$> thsDyn

includeThing :: Map ThingName ThingScope -> ThingName -> Bool
includeThing scopes name =
  Map.member name scopes && Calendar.includeInSidebar name