module Ka.Plugin.ViewNote
  ( runPlugin,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Route (Route (..))
import Ka.View (renderPandoc, renderThinkLink)
import Reflex.Dom.Core hiding (Link)
import Reflex.Dom.Pandoc.Document
  ( PandocBuilder,
  )
import Text.Pandoc.Definition (Pandoc (Pandoc))

runPlugin ::
  forall t m.
  ( Reflex t,
    MonadHold t m,
    MonadIO m,
    PandocBuilder t m
  ) =>
  Dynamic t Graph ->
  Dynamic t (Map G.Thing Pandoc) ->
  (Event t (Map G.Thing (Maybe Pandoc))) ->
  m (Event t (Map G.Thing (Maybe (m (Event t Route)))))
runPlugin graphD pandocD pandocE = do
  -- Like `pandocE` but includes other notes whose backlinks have changed as a
  -- result of the update in `pandocE`
  let pandocAllE :: Event t (Map G.Thing (Maybe Pandoc)) =
        ffor
          -- `attach` gets us the old graph. The promptly version gets the new
          -- one (updated in this frame)
          ( attach (current graphD) $
              attachPromptlyDyn (zipDyn graphD pandocD) pandocE
          )
          $ \(oldGraph, ((graph, docMap), fps)) ->
            Map.unions $
              ffor (Map.toList fps) $ \(fp, doc) ->
                let -- Consider only edges that were removed, modified or added.
                    esDirty =
                      symmetricDifference
                        (Set.fromList $ G.postSetWithLabel fp graph)
                        (Set.fromList $ G.postSetWithLabel fp oldGraph)
                    esR =
                      fforMaybe (Set.toList esDirty) $ \(fp', _ctx) -> do
                        -- Add linked doc not already marked as changed.
                        guard $ Map.notMember fp' fps
                        doc' <- Map.lookup fp' docMap
                        pure (fp', Just doc')
                 in Map.insert fp doc $ Map.fromList esR
  pure $
    ffor (attachPromptlyDyn graphD pandocAllE) $ \(graph, xs) ->
      Map.fromList $
        ffor (Map.toList xs) $ \(fp, mdoc) -> do
          case mdoc of
            Nothing -> (fp, Nothing)
            Just doc -> (fp, Just $ render graph fp doc)

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference x y =
  (x `Set.union` y) `Set.difference` (x `Set.intersection` y)

render ::
  (PandocBuilder t m, MonadIO m) =>
  Graph ->
  G.Thing ->
  Pandoc ->
  m (Event t Route)
render g k doc = do
  let backlinks =
        -- FIXME: backlinks order is lost
        G.preSetWithLabel k g
  r1 <- divClass "ui basic segment" $ do
    elClass "h1" "ui header" $ text $ G.unThing k
    renderPandoc doc
  r2 <- divClass "ui backlinks segment" $ do
    backlinksWidget backlinks
  pure $ leftmost [r1, r2]

backlinksWidget ::
  PandocBuilder t m =>
  [(G.Thing, [G.Context])] ->
  m (Event t Route)
backlinksWidget xs = do
  elClass "h2" "header" $ text "Backlinks"
  divClass "" $ do
    fmap leftmost $
      forM xs $ \(x, blks) -> do
        divClass "ui vertical segment" $ do
          evt1 <- elAttr "h3" ("class" =: "header") $ do
            renderThinkLink x
          evt2 <- elClass "ul" "ui list context" $ do
            fmap leftmost $
              forM blks $ \blk -> do
                let blkDoc = Pandoc mempty (one blk)
                el "li" $
                  renderPandoc blkDoc
          pure $ leftmost [evt1, evt2]
