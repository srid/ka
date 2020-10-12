module Ka.View
  ( headWidget,
  )
where

import Clay (em, pct, (?))
import qualified Clay as C
import qualified Ka.PandocView as PandocView
import Reflex.Dom.Core

style :: C.Css
style = do
  ".ui.container" ? do
    let linkColor = C.purple
    "a.route" ? do
      C.important $ do
        C.fontWeight C.bold
        C.color linkColor
        C.cursor C.pointer
    "a.route:hover" ? do
      C.important $ do
        C.backgroundColor linkColor
        C.color C.white
    ".backlinks" ? do
      let smallerFont x = C.important $ C.fontSize x
      C.backgroundColor "#eee"
      "h2" ? smallerFont (em 1.2)
      "h3" ? smallerFont (pct 90)
      ".context" ? smallerFont (pct 85)
      C.color C.gray
      do
        let linkColorFaded = "#555"
        C.a ? do
          C.important $ do
            C.color linkColorFaded
        C.a C.# C.hover ? do
          C.important $ do
            C.backgroundColor linkColorFaded
            C.color C.white

headWidget :: DomBuilder t m => m ()
headWidget = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("content" =: "width=device-width, initial-scale=1" <> "name" =: "viewport") blank
  elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.7/dist/semantic.min.css") blank
  el "style" $ do
    text $
      toStrict $
        C.render $ do
          style
          PandocView.style
