{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ka.Plugin.ViewNote
  ( runPlugin,
  )
where

import Clay (em, pct, px, (?))
import qualified Clay as C
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Markdown (noteFileTitle)
import Ka.Route (Route (..), routeLink)
import qualified Ka.View as V
import Reflex.Dom.Core hiding (Link)
import Reflex.Dom.Pandoc (Config (Config), elPandocRawSafe)
import Reflex.Dom.Pandoc.Document
  ( PandocBuilder,
    PandocRaw (..),
    defaultConfig,
    elPandoc,
  )
import System.Directory (makeAbsolute)
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
  Dynamic t (Map FilePath Pandoc) ->
  (Event t (Map FilePath (Maybe Pandoc))) ->
  m (Event t (Map FilePath (Maybe (m (Event t Route)))))
runPlugin graphD pandocD pandocE = do
  -- Like `pandocE` but includes other notes whose backlinks have changed as a
  -- result of the update in `pandocE`
  let pandocAllE :: Event t (Map FilePath (Maybe Pandoc)) =
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
          let htmlFile = V.mdToHtml fp
          case mdoc of
            Nothing -> (htmlFile, Nothing)
            Just doc -> (htmlFile, Just $ render graph fp doc)

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference x y =
  (x `Set.union` y) `Set.difference` (x `Set.intersection` y)

render ::
  (PandocBuilder t m, MonadIO m) =>
  Graph ->
  FilePath ->
  Pandoc ->
  m (Event t Route)
render g k doc = do
  kAbs <- liftIO $ makeAbsolute k
  let backlinks =
        -- FIXME: backlinks order is lost
        G.preSetWithLabel k g
  noteWidget k kAbs (V.rewriteLinks doc) backlinks

_style :: C.Css
_style = do
  ".ui.container" ? do
    C.a ? do
      -- TODO: Extend reflex-dom-pandoc to set custom attriutes on elements
      -- Like table,a. Then style only wikilinks.
      C.important $ do
        C.fontWeight C.bold
        C.color C.green
    C.a C.# C.hover ? do
      C.important $ do
        C.backgroundColor C.green
        C.color C.white
    ".backlinks" ? do
      let smallerFont x = C.important $ C.fontSize x
      C.backgroundColor "#eee"
      "h2" ? smallerFont (em 1.2)
      "h3" ? smallerFont (pct 90)
      ".context" ? smallerFont (pct 85)
      C.color C.gray
      do
        let linkColor = "#555"
        C.a ? do
          C.important $ do
            C.color linkColor
        C.a C.# C.hover ? do
          C.important $ do
            C.backgroundColor linkColor
            C.color C.white
    -- Pandoc styles
    "#footnotes" ? do
      C.fontSize $ pct 85
      C.borderTop C.solid (px 1) C.black

-- | Route monoid for use with reflex-dom-pandoc
newtype RouteM t = RouteM
  {unRouteM :: Event t Route}

instance Reflex t => Semigroup (RouteM t) where
  RouteM a <> RouteM b = RouteM $ leftmost [a, b]

instance Reflex t => Monoid (RouteM t) where
  mempty = RouteM never

noteWidget ::
  PandocBuilder t m =>
  FilePath ->
  FilePath ->
  Pandoc ->
  [(FilePath, [Block])] ->
  m (Event t Route)
noteWidget fp fpAbs doc backlinks = do
  -- V.kaTemplate style (text $ noteFileTitle fp) $ do
  r1 <- divClass "ui basic segment" $ do
    elClass "h1" "ui header" $ text $ noteFileTitle fp
    let pandocCfg = Config $ \_ url _inlines -> do
          fmap RouteM $
            routeLink (Route_Node $ toString url) $
              text "OOHH"
    fmap unRouteM $ elPandoc pandocCfg doc
  r2 <- divClass "ui backlinks segment" $ do
    backlinksWidget backlinks
  divClass "ui center aligned basic segment" $ do
    let editUrl = toText $ "vscode://file" <> fpAbs
    elAttr "a" ("href" =: editUrl) $ text "Edit locally"
    text " | "
    elAttr "a" ("href" =: ".") $ text "Index"
  pure $ leftmost [r1, r2]

backlinksWidget ::
  PandocBuilder t m =>
  [(FilePath, [Block])] ->
  m (Event t Route)
backlinksWidget xs = do
  elClass "h2" "header" $ text "Backlinks"
  divClass "" $ do
    fmap leftmost $
      forM xs $ \(x, blks) -> do
        divClass "ui vertical segment" $ do
          evt <- elAttr "h3" ("class" =: "header") $ do
            routeLink (Route_Node $ V.mdToHtml x) $
              text $ noteFileTitle x
          elClass "ul" "ui list context" $ do
            forM_ blks $ \blk -> do
              let blkDoc = V.rewriteLinks $ Pandoc mempty (one blk)
              el "li" $
                elPandoc defaultConfig blkDoc
          pure evt