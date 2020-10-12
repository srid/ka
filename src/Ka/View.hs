module Ka.View
  ( mdToHtml,
    rewriteLinks,
    kaTemplate,
  )
where

import qualified Clay as C
import qualified Data.Text as T
import Ka.Markdown (getNoteLink)
import Reflex.Dom.Core hiding (Link)
import System.FilePath ((-<.>))
import Text.Pandoc.Definition (Inline (Link), Pandoc)
import qualified Text.Pandoc.Walk as W

-- Rewrite *.md -> *.html in links
rewriteLinks :: Pandoc -> Pandoc
rewriteLinks =
  W.walk $ \x ->
    case getNoteLink x of
      Nothing -> x
      Just (attr, inlines, (toString -> url, title)) ->
        Link attr inlines (toText $ mdToHtml url, title)

mdToHtml :: FilePath -> FilePath
mdToHtml = replaceWhitespace . (-<.> ".html")
  where
    replaceWhitespace =
      toString . T.replace " " "_" . toText

kaTemplate :: DomBuilder t m => C.Css -> m () -> m a -> m a
kaTemplate style titleW w = do
  el "!DOCTYPE html" $ blank
  elAttr "html" ("lang" =: "en") $ do
    el "head" $ do
      elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
      elAttr "meta" ("content" =: "width=device-width, initial-scale=1" <> "name" =: "viewport") blank
      elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.7/dist/semantic.min.css") blank
      el "style" $ do
        text $ toStrict $ C.render style
      el "title" $ titleW
    el "body" $ do
      divClass "ui text container" w
