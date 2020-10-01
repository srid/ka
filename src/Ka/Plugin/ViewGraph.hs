module Ka.Plugin.ViewGraph where

import Ka.Diff
import Ka.Plugin
import Reflex.Dom.Core
import Shower (shower)

viewGraphPlugin :: Plugin
viewGraphPlugin =
  def
    { fileGenerator = \g _docs ->
        one ("index.html", Modified $ fmap snd $ renderStatic $ debugWidget g)
    }

debugWidget :: (DomBuilder t m, Show a) => a -> m ()
debugWidget x = do
  el "code" $ do
    el "pre" $ do
      text $ toText $ shower x
