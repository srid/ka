module Ka.Plugin.ViewNote
  ( render,
    mdToHtml,
  )
where

import Ka.Graph (Graph)
import qualified Ka.Graph as G
import Ka.Markdown (getNoteLink, noteFileTitle)
import Reflex.Dom.Core hiding (Link)
import Reflex.Dom.Pandoc.Document
  ( PandocBuilder,
    defaultConfig,
    elPandoc,
  )
import System.Directory (makeAbsolute)
import System.FilePath ((-<.>))
import Text.Pandoc.Definition (Block, Inline (Link), Pandoc (Pandoc))
import qualified Text.Pandoc.Walk as W

render :: Graph -> FilePath -> Pandoc -> IO ByteString
render g k doc =
  fmap snd $
    renderStatic $ do
      kAbs <- liftIO $ makeAbsolute k
      let backlinks =
            G.preSetWithLabel k g
      noteWidget k kAbs (rewriteLinks doc) backlinks

-- Rewrite *.md -> *.html in links
rewriteLinks :: Pandoc -> Pandoc
rewriteLinks =
  W.walk $ \x ->
    case getNoteLink x of
      Nothing -> x
      Just (attr, inlines, (toString -> url, title)) ->
        Link attr inlines (toText $ mdToHtmlUrl url, title)

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
  [(FilePath, [Block])] ->
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
      text ".backlinks .context { color: gray; }"
      text ".backlinks .context a { color: gray; }"
    el "title" $ text $ noteFileTitle fp
  el "body" $ do
    divClass "ui text container" $ do
      -- divClass "ui basic segment" $
      divClass "ui basic segment" $ do
        elClass "h1" "ui header" $ text $ noteFileTitle fp
        elPandoc defaultConfig doc
      divClass "ui stacked backlinks segment" $ do
        backlinksWidget backlinks
      divClass "ui center aligned basic segment" $ do
        let editUrl = toText $ "vscode://file" <> fpAbs
        elAttr "a" ("href" =: editUrl) $ text "Edit locally"
        text " | "
        elAttr "a" ("href" =: ".") $ text "Index"

backlinksWidget :: PandocBuilder t m => [(FilePath, [Block])] -> m ()
backlinksWidget xs = do
  whenNotNull xs $ \_ -> do
    elClass "b" "header" $ text "Backlinks"
    elClass "ul" "ui list" $ do
      forM_ xs $ \(x, blks) -> do
        elAttr "a" ("class" =: "item" <> "href" =: toText (mdToHtmlUrl x)) $
          text $ noteFileTitle x
        elClass "ul" "context" $ do
          forM_ blks $ \blk -> do
            let blkDoc = rewriteLinks $ Pandoc mempty (one blk)
            el "li" $
              elPandoc defaultConfig blkDoc