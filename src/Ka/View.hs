module Ka.View
  ( headWidget,
  )
where

import Clay (em, pct, px, (?))
import qualified Clay as C
import Reflex.Dom.Core hiding (Link)

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

headWidget :: DomBuilder t m => m ()
headWidget = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("content" =: "width=device-width, initial-scale=1" <> "name" =: "viewport") blank
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.7/dist/semantic.min.css") blank
  el "style" $ do
    text $ toStrict $ C.render style
