module Ka.Plugin where

import Data.Dependent.Sum (DSum (..))
import Ka.Graph (Graph)
import qualified Ka.Graph as G
import qualified Ka.Plugin.Calendar as Calendar
import qualified Ka.Plugin.ViewNote as ViewNote
import Ka.Route (Route)
import Ka.View (renderPandoc, renderThinkLink)
import Reflex
import Reflex.Dom
import Reflex.Dom.Pandoc (PandocBuilder)
import Text.Pandoc.Definition (Pandoc (Pandoc))

data Doc a where
  Doc_Pandoc :: Doc Pandoc
  Doc_Calendar :: Doc (Set G.Thing)

type D t = Dynamic t (Map G.Thing (DSum Doc Identity))

renderDoc :: PandocBuilder t m => Graph -> G.Thing -> DSum Doc Identity -> m (Event t Route)
renderDoc g th v = do
  r1 <- case v of
    Doc_Pandoc :=> Identity doc ->
      ViewNote.render g th doc
    Doc_Calendar :=> Identity days ->
      Calendar.render days
  r2 <- divClass "ui backlinks segment" $ do
    let backlinks = G.preSetWithLabel th g
    backlinksWidget backlinks
  pure $ leftmost [r1, r2]

backlinksWidget ::
  PandocBuilder t m =>
  [(G.Thing, [G.Context])] ->
  m (Event t Route)
backlinksWidget xs = do
  elClass "h2" "header" $ text "Backlinks"
  divClass "" $ do
    fmap leftmost $
      forM xs $ \(x, blks) -> do
        divClass "ui vertical segment" $ do
          evt1 <- elAttr "h3" ("class" =: "header") $ do
            renderThinkLink x
          evt2 <- elClass "ul" "ui list context" $ do
            fmap leftmost $
              forM blks $ \blk -> do
                let blkDoc = Pandoc mempty (one blk)
                el "li" $
                  renderPandoc blkDoc
          pure $ leftmost [evt1, evt2]
