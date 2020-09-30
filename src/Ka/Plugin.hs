module Ka.Plugin where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import Data.Default (Default)
import qualified Data.Map.Strict as Map
import Ka.Diff (Changed (..), V (..))
import Ka.Graph (Graph)
import Reflex.Dom.Core
import Reflex.Dom.Pandoc.Document
import Shower (shower)
import System.FilePath ((-<.>))
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc)

type CMSyntaxSpec =
  CM.SyntaxSpec
    (Either CM.ParseError)
    (CP.Cm () B.Inlines)
    (CP.Cm () B.Blocks)

data Plugin = Plugin
  { -- | Custom extensions to apply when parsing Markdown
    commonmarkSpec :: CMSyntaxSpec,
    -- | Transform Pandoc type before graph creation
    docTransformer :: (Pandoc -> Pandoc),
    -- | Transform Pandoc type after graph creation
    docTransformerWithGraph :: (() -> Pandoc -> Pandoc),
    -- | Files to generate
    fileGenerator :: Graph -> Map FilePath (V Pandoc) -> Map FilePath (Changed (IO ByteString))
  }

instance Default Plugin where
  def =
    Plugin mempty id (const id) (const mempty)

wipPlugin :: Plugin
wipPlugin =
  def
    { fileGenerator = \g docs ->
        let pages = Map.fromList $
              catMaybes $
                flip fmap (Map.toList docs) $ \(k, v) -> case v of
                  VChanged ch ->
                    Just $
                      (k -<.> ".html",) $
                        flip fmap ch $ \doc ->
                          renderReflexWidget $ noteWidget doc
                  VSame _ ->
                    Nothing
            indexPage =
              one ("index.html", Modified $ renderReflexWidget $ debugWidget g)
         in pages <> indexPage
    }

-- Render plugin

debugWidget :: (DomBuilder t m, Show a) => a -> m ()
debugWidget x = do
  el "code" $ do
    el "pre" $ do
      text $ toText $ shower x

renderReflexWidget :: forall x. StaticWidget x () -> IO ByteString
renderReflexWidget w =
  fmap snd $ renderStatic w

noteWidget :: PandocBuilder t m => Pandoc -> m ()
noteWidget doc =
  elPandoc defaultConfig doc