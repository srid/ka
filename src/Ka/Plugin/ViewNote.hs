module Ka.Plugin.ViewNote where

import qualified Data.Map.Strict as Map
import Ka.Diff
import Ka.Plugin
import Reflex.Dom.Core
import Reflex.Dom.Pandoc.Document
  ( PandocBuilder,
    defaultConfig,
    elPandoc,
  )
import System.FilePath ((-<.>))
import Text.Pandoc.Definition (Pandoc)

viewNotePlugin :: Plugin
viewNotePlugin =
  def
    { fileGenerator = \_g docs ->
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