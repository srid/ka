module Ka.Plugin.ViewNote
  ( render,
    mdToHtml,
  )
where

import Clay (em, pct, px, (?))
import qualified Clay as C
import qualified Data.Text as T
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
mdToHtml = replaceWhitespace . (-<.> ".html")
  where
    replaceWhitespace =
      toString . T.replace " " "_" . toText

mdToHtmlUrl :: FilePath -> FilePath
mdToHtmlUrl =
  -- The ./ prefix is to prevent the browser from thinking that our URL is a
  -- custom protocol when it contains a colon.
  ("./" <>) . mdToHtml

style :: C.Css
style = do
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
    elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.7/dist/semantic.min.css") blank
    el "style" $ do
      text $ toStrict $ C.render style
    el "title" $ text $ noteFileTitle fp
  el "body" $ do
    divClass "ui text container" $ do
      divClass "ui basic segment" $ do
        elClass "h1" "ui header" $ text $ noteFileTitle fp
        elPandoc defaultConfig doc
      divClass "ui backlinks segment" $ do
        backlinksWidget backlinks
      divClass "ui center aligned basic segment" $ do
        let editUrl = toText $ "vscode://file" <> fpAbs
        elAttr "a" ("href" =: editUrl) $ text "Edit locally"
        text " | "
        elAttr "a" ("href" =: ".") $ text "Index"

backlinksWidget :: PandocBuilder t m => [(FilePath, [Block])] -> m ()
backlinksWidget xs = do
  whenNotNull xs $ \_ -> do
    elClass "h2" "header" $ text "Backlinks"
    divClass "" $ do
      forM_ xs $ \(x, blks) -> do
        divClass "ui vertical segment" $ do
          elAttr "h3" ("class" =: "header") $ do
            elAttr "a" ("href" =: toText (mdToHtmlUrl x)) $
              text $ noteFileTitle x
          elClass "ul" "ui list context" $ do
            forM_ blks $ \blk -> do
              let blkDoc = rewriteLinks $ Pandoc mempty (one blk)
              el "li" $
                elPandoc defaultConfig blkDoc