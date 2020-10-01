module Ka.Plugin where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import Data.Default (Default)
import Ka.Diff (Changed (..))
import Ka.Graph (Graph)
import Reflex.Dom.Core
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Pandoc)

type CMSyntaxSpec =
  CM.SyntaxSpec
    (Either CM.ParseError)
    (CP.Cm () B.Inlines)
    (CP.Cm () B.Blocks)

-- NOTE: Plugin API is WIP, and not finalized.
data Plugin = Plugin
  { -- | Custom extensions to apply when parsing Markdown
    commonmarkSpec :: CMSyntaxSpec,
    -- | Transform Pandoc type before graph creation
    --
    -- Note: Link changes done here will affect the graph. Use
    -- @docTransformerWithGraph@ if that is undesired.
    docTransformer :: (Pandoc -> Pandoc),
    -- | Transform Pandoc type after graph creation
    docTransformerWithGraph :: (Graph -> Pandoc -> Pandoc),
    -- | Determine what other files should be marked as modified
    --
    -- This depends on the subsequent actions using the graph.
    -- The old graph should be used for edge-removal cases.
    -- TODO: Instead of passing the old graph, just send a (V Graph)
    docTouches :: Graph -> Graph -> Map FilePath (Changed Pandoc) -> Set FilePath,
    -- | Files to generate
    fileGenerator :: Graph -> Map FilePath (Changed Pandoc) -> Map FilePath (Changed (IO ByteString))
  }

instance Default Plugin where
  def =
    Plugin mempty id (const id) (const mempty) (const mempty)
