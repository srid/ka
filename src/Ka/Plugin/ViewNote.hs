module Ka.Plugin.ViewNote where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ka.Markdown (getNoteLink)
import Ka.Plugin
import Reflex.Dom.Core hiding (Link)
import Reflex.Dom.Pandoc.Document
  ( PandocBuilder,
    defaultConfig,
    elPandoc,
  )
import System.FilePath ((-<.>))
import Text.Pandoc.Definition (Inline (Link), Pandoc)
import qualified Text.Pandoc.Walk as W

viewNotePlugin :: Plugin
viewNotePlugin =
  def
    { docTransformerWithGraph = \_g doc ->
        -- Rewrite links to point to the generated HTML page
        flip W.walk doc $ \x ->
          case getNoteLink x of
            Nothing -> x
            Just (attr, inlines, (toString -> url, title)) ->
              Link attr inlines (toText $ mdToHtml url, title),
      docTouches = \g docs ->
        -- Mark frontlinks of modified notes as modified, because their
        -- backlinks would have been changed.
        -- FIXME: !! need old graph to know postSet that got *removed*
        -- Should be done by adding V to edge label of graph?
        Set.unions $
          Map.keys docs <&> \fp ->
            AM.postSet fp g,
      fileGenerator = \g docs ->
        -- Generate .html for each note file
        Map.fromList $
          Map.toList docs <&> \(k, ch) ->
            (mdToHtml k,) $
              ch <&> \doc ->
                fmap snd $
                  renderStatic $ do
                    noteWidget doc
                    backlinksWidget $ AM.postSet k g
    }

mdToHtml :: FilePath -> FilePath
mdToHtml = (-<.> ".html")

noteWidget :: PandocBuilder t m => Pandoc -> m ()
noteWidget doc =
  elPandoc defaultConfig doc

backlinksWidget :: DomBuilder t m => Set FilePath -> m ()
backlinksWidget xs = do
  el "hr" blank
  el "h2" $ text "Backlinks"
  elClass "ul" "backlinks" $ do
    forM_ xs $ \x -> do
      el "li" $ do
        elAttr "a" ("href" =: toText (mdToHtml x)) $ text (toText x)