module Ka.Plugin where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import Data.Default (Default)
import qualified Data.Map.Strict as Map
import GHC.IO.Unsafe (unsafePerformIO)
import Ka.Diff (Changed (..), V (..))
import Reflex.Dom.Core
import Reflex.Dom.Pandoc.Document
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
    fileGenerator :: Map FilePath (V Pandoc) -> Map FilePath (Changed Text)
  }

instance Default Plugin where
  def =
    Plugin mempty id (const id) (const mempty)

demoPlugin :: Plugin
demoPlugin =
  def
    { commonmarkSpec = CM.defaultSyntaxSpec,
      fileGenerator = \docs ->
        Map.fromList $
          catMaybes $
            flip fmap (Map.toList docs) $ \(k, v) -> case v of
              VChanged ch ->
                Just $
                  (k -<.> ".html",) $
                    flip fmap ch $ \doc ->
                      -- TODO: allow IO in fileGenerated instead
                      unsafePerformIO $ fmap (decodeUtf8 . snd) $ renderStatic $ noteWidget doc
              VSame _ ->
                Nothing
    }

noteWidget :: PandocBuilder t m => Pandoc -> m ()
noteWidget doc =
  elPandoc defaultConfig doc