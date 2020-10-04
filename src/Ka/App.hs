{-# LANGUAGE RecursiveDo #-}

module Ka.App where

import qualified Algebra.Graph.AdjacencyMap as AM
import Commonmark (defaultSyntaxSpec)
import qualified Commonmark.Extensions as CE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Markdown (noteExtension, parseMarkdown, queryLinks)
import qualified Ka.Plugin.ViewNote as ViewNote
import Ka.Plugin.WikiLink (wikiLinkSpec)
import Ka.Watch (Status (..), directoryFilesContent)
import Reflex hiding (mapMaybe)
import Reflex.Host.Headless (MonadHeadlessApp)
import Text.Pandoc.Definition (Pandoc)

kaApp ::
  forall t m.
  MonadHeadlessApp t m =>
  m (Event t (Map FilePath (Maybe (IO ByteString))))
kaApp = do
  fileContentE <- directoryFilesContent "." noteExtension
  let pandocE :: Event t (Map FilePath (Pandoc, Status)) =
        ffor fileContentE $
          Map.mapWithKey $ \fp (s, st) ->
            let spec =
                  defaultSyntaxSpec
                    <> wikiLinkSpec
                    <> CE.footnoteSpec
                    <> CE.gfmExtensions
                doc = parseMarkdown spec fp s
             in (doc, st)
  graphD :: Dynamic t Graph <-
    foldDyn G.patch G.empty $
      ffor pandocE $
        Map.map (first $ toList . queryLinks)
  pandocD :: Dynamic t (Map FilePath Pandoc) <-
    foldDyn patchMap mempty $
      ffor pandocE $
        Map.map $ \(doc, st) ->
          if st == Deleted
            then Nothing
            else Just doc
  pluginViewNotes graphD pandocD pandocE

-- TODO: Extract the below as plugins once FRP API stablizes.

pluginViewNotes ::
  forall t m.
  ( Reflex t,
    MonadHold t m
  ) =>
  Dynamic t Graph ->
  Dynamic t (Map FilePath Pandoc) ->
  (Event t (Map FilePath (Pandoc, Status))) ->
  m (Event t (Map FilePath (Maybe (IO ByteString))))
pluginViewNotes graphD pandocD pandocE = do
  -- Like `pandocE` but includes other notes whose backlinks have changed as a
  -- result of the update in `pandocE`
  let pandocAllE :: Event t (Map FilePath (Pandoc, Status)) =
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
                        (AM.postSet fp graph)
                        (AM.postSet fp oldGraph)
                    esR =
                      fforMaybe (Set.toList esDirty) $ \fp' -> do
                        -- Add linked doc not already marked as changed.
                        guard $ Map.notMember fp' fps
                        doc' <- Map.lookup fp' docMap
                        pure (fp', (doc', Dirty))
                 in Map.insert fp doc $ Map.fromList esR
  pure $
    ffor (attachPromptlyDyn graphD pandocAllE) $ \(graph, xs) ->
      Map.fromList $
        ffor (Map.toList xs) $ \(fp, (doc, st)) -> do
          let htmlFile = ViewNote.mdToHtml fp
          case st of
            Deleted -> (htmlFile, Nothing)
            _ -> (htmlFile, Just $ ViewNote.render graph fp doc)

patchMap :: Ord k => Map k (Maybe a) -> Map k a -> Map k a
patchMap diff xs =
  let (toAdd, toDel) = Map.mapEither (maybeToLeft ()) diff
   in Map.union toAdd xs `Map.difference` toDel

symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference x y =
  (x `Set.union` y) `Set.difference` (x `Set.intersection` y)