module Ka.Plugin.ViewNote
  ( runPlugin,
    render,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Route (Route (..))
import Ka.View (renderPandoc)
import Reflex.Dom.Core
import Reflex.Dom.Pandoc.Document
  ( PandocBuilder,
  )
import Text.Pandoc.Definition (Pandoc)

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
  m (Event t (Map G.Thing (Maybe Pandoc)))
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
    ffor pandocAllE $ \xs ->
      Map.fromList $
        ffor (Map.toList xs) $ \(fp, mdoc) -> do
          case mdoc of
            Nothing -> (fp, Nothing)
            Just doc -> (fp, Just doc)
  where
    symmetricDifference :: Ord a => Set a -> Set a -> Set a
    symmetricDifference x y =
      (x `Set.union` y) `Set.difference` (x `Set.intersection` y)

render ::
  PandocBuilder t m =>
  Graph ->
  G.Thing ->
  Pandoc ->
  m (Event t Route)
render _g k doc = do
  divClass "ui basic segment" $ do
    elClass "h1" "ui header" $ text $ G.unThing k
    renderPandoc doc
