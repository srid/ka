{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ka.Plugin.ViewNote
  ( runPlugin,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Markdown (mdFileThing)
import Ka.Route (Route (..), routeLink)
import Reflex.Dom.Core hiding (Link)
import Reflex.Dom.Pandoc (Config (Config), elPandocRawSafe)
import Reflex.Dom.Pandoc.Document
  ( PandocBuilder,
    PandocRaw (..),
    elPandoc,
  )
import Text.Pandoc.Definition (Block, Pandoc (Pandoc))

instance PandocRaw (HydrationDomBuilderT s t m) where
  type PandocRawConstraints (HydrationDomBuilderT s t m) = (DomBuilder t (HydrationDomBuilderT s t m))
  elPandocRaw = elPandocRawSafe

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
  noteWidget k doc backlinks

-- | Route monoid for use with reflex-dom-pandoc
newtype RouteM t = RouteM
  {unRouteM :: Event t Route}

instance Reflex t => Semigroup (RouteM t) where
  RouteM a <> RouteM b = RouteM $ leftmost [a, b]

instance Reflex t => Monoid (RouteM t) where
  mempty = RouteM never

noteWidget ::
  PandocBuilder t m =>
  G.Thing ->
  Pandoc ->
  [(G.Thing, [Block])] ->
  m (Event t Route)
noteWidget fp doc backlinks = do
  r1 <- divClass "ui basic segment" $ do
    elClass "h1" "ui header" $ text $ G.unThing fp
    renderPandoc doc
  r2 <- divClass "ui backlinks segment" $ do
    backlinksWidget backlinks
  divClass "ui center aligned basic segment" $ do
    elAttr "a" ("href" =: ".") $ text "Index"
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
            renderLink x
          evt2 <- elClass "ul" "ui list context" $ do
            fmap leftmost $
              forM blks $ \blk -> do
                let blkDoc = Pandoc mempty (one blk)
                el "li" $
                  renderPandoc blkDoc
          pure $ leftmost [evt1, evt2]

renderPandoc :: (PandocBuilder t m) => Pandoc -> m (Event t Route)
renderPandoc doc = do
  let pandocCfg = Config $ \_ url _inlines -> do
        fmap RouteM $
          renderLink $ mdFileThing $ toString url
  fmap unRouteM $ elPandoc pandocCfg doc

renderLink :: DomBuilder t m => G.Thing -> m (Event t Route)
renderLink x = do
  routeLink (Route_Node x) $ do
    text $ G.unThing x