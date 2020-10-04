module Ka.Plugin.ViewNote
  ( render,
    mdToHtml,
  )
where

import qualified Algebra.Graph.AdjacencyMap as AM
import qualified Data.Set as Set
import Ka.Graph (Graph)
import Ka.Markdown (getNoteLink, noteFileTitle)
import Reflex.Dom.Core hiding (Link)
import Reflex.Dom.Pandoc.Document
  ( PandocBuilder,
    defaultConfig,
    elPandoc,
  )
import System.Directory (makeAbsolute)
import System.FilePath ((-<.>))
import Text.Pandoc.Definition (Inline (Link), Pandoc)
import qualified Text.Pandoc.Walk as W

render :: Graph -> FilePath -> Pandoc -> IO ByteString
render g k doc =
  let docWLinksFixed =
        flip W.walk doc $ \x ->
          case getNoteLink x of
            Nothing -> x
            Just (attr, inlines, (toString -> url, title)) ->
              Link attr inlines (toText $ mdToHtmlUrl url, title)
   in fmap snd $
        renderStatic $ do
          kAbs <- liftIO $ makeAbsolute k
          noteWidget k kAbs docWLinksFixed $ AM.preSet k g

mdToHtml :: FilePath -> FilePath
mdToHtml = (-<.> ".html")

mdToHtmlUrl :: FilePath -> FilePath
mdToHtmlUrl =
  -- The ./ prefix is to prevent the browser from thinking that our URL is a
  -- custom protocol when it contains a colon.
  ("./" <>) . mdToHtml

noteWidget ::
  PandocBuilder t m =>
  FilePath ->
  FilePath ->
  Pandoc ->
  Set FilePath ->
  m ()
noteWidget fp fpAbs doc backlinks = do
  el "head" $ do
    elAttr "meta" ("content" =: "width=device-width, initial-scale=1" <> "name" =: "viewport") blank
    -- TODO: Extend reflex-dom-pandoc to set custom attriutes on elements
    -- Like table,a. Then style only zettel links.
    elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.7/dist/semantic.min.css") blank
    el "style" $ do
      text ".ui.container a { font-weight: bold; }"
      text ".ui.container { zoom: 1.05; margin: 0.5em auto; }"
      text "div#footnotes { font-size: 85%; border-top: 1px solid grey;}"
    el "title" $ text $ noteFileTitle fp
  el "body" $ do
    divClass "ui text container" $ do
      -- divClass "ui basic segment" $
      divClass "ui basic segment" $ do
        elClass "h1" "ui header" $ text $ noteFileTitle fp
        elPandoc defaultConfig doc
      divClass "ui stacked segment" $ do
        backlinksWidget backlinks
      divClass "ui center aligned basic segment" $ do
        let editUrl = toText $ "vscode://file" <> fpAbs
        elAttr "a" ("href" =: editUrl) $ text "Edit locally"
        text " | "
        elAttr "a" ("href" =: ".") $ text "Index"

backlinksWidget :: DomBuilder t m => Set FilePath -> m ()
backlinksWidget (Set.toList -> xs) = do
  whenNotNull xs $ \_ -> do
    elClass "b" "header" $ text "Backlinks"
    elClass "ul" "ui list" $ do
      forM_ xs $ \x -> do
        elAttr "a" ("class" =: "item" <> "href" =: toText (mdToHtmlUrl x)) $
          text $ noteFileTitle x