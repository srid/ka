module Ka.Plugin.ViewNote where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Ka.Markdown (getNoteLink, noteFileTitle)
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
        -- FIXME: See note in Plugin.hs
        flip W.walk doc $ \x ->
          case getNoteLink x of
            Nothing -> x
            Just (attr, inlines, (toString -> url, title)) ->
              Link attr inlines (toText $ mdToHtmlUrl url, title),
      docTouches = \oldG g docs ->
        -- Mark frontlinks of modified notes as modified, because their
        -- backlinks would have been changed.
        Set.unions $
          Map.keys docs <&> \fp ->
            AM.postSet fp g `Set.union` AM.postSet fp oldG,
      fileGenerator = \g docs ->
        -- Generate .html for each note file
        Map.fromList $
          Map.toList docs <&> \(k, ch) ->
            (mdToHtml k,) $
              ch <&> \doc ->
                fmap snd $
                  renderStatic $ do
                    noteWidget k doc $ AM.preSet k g
    }

mdToHtml :: FilePath -> FilePath
mdToHtml = (-<.> ".html")

mdToHtmlUrl :: FilePath -> FilePath
mdToHtmlUrl =
  -- The ./ prefix is to prevent the browser from thinking that our URL is a
  -- custom protocol when it contains a colon.
  ("./" <>) . mdToHtml

noteWidget :: PandocBuilder t m => FilePath -> Pandoc -> Set FilePath -> m ()
noteWidget fp doc backlinks = do
  el "head" $ do
    el "style" $ do
      text "a { color: green; text-decoration: none; } a:hover { background-color: green; color: white; }"
    el "title" $ text $ noteFileTitle fp
  elAttr "div" ("style" =: "max-width: 760px; margin: 0 auto;") $ do
    el "h1" $ text $ noteFileTitle fp
    el "hr" blank
    elPandoc defaultConfig doc
    backlinksWidget backlinks

backlinksWidget :: DomBuilder t m => Set FilePath -> m ()
backlinksWidget xs = do
  el "hr" blank
  el "h2" $ text "Backlinks"
  elClass "ul" "backlinks" $ do
    forM_ xs $ \x -> do
      el "li" $ do
        elAttr "a" ("href" =: toText (mdToHtmlUrl x)) $
          text $ noteFileTitle x