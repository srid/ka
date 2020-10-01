module Ka.Plugin.ViewNote where

import qualified Data.Map.Strict as Map
import Ka.Diff
import Ka.Markdown (getNoteLink)
import Ka.Plugin
import Reflex.Dom.Core (def, renderStatic)
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
        flip W.walk doc $ \x ->
          case getNoteLink x of
            Nothing -> x
            Just (attr, inlines, (toString -> url, title)) ->
              Link attr inlines (toText $ url -<.> ".html", title),
      fileGenerator = \_g docs ->
        Map.fromList $
          catMaybes $
            flip fmap (Map.toList docs) $ \(k, v) -> case v of
              VChanged ch ->
                Just $
                  (k -<.> ".html",) $
                    flip fmap ch $ \doc ->
                      fmap snd $ renderStatic $ noteWidget doc
              VSame _ ->
                Nothing
    }

noteWidget :: PandocBuilder t m => Pandoc -> m ()
noteWidget doc =
  elPandoc defaultConfig doc