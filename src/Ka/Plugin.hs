module Ka.Plugin where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
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
    docTransformerWithGraph :: (() -> Pandoc -> Pandoc)
  }

demoPlugin :: Plugin
demoPlugin =
  Plugin
    { commonmarkSpec = mempty,
      docTransformer = id,
      docTransformerWithGraph = const id
    }