module Ka.Plugin.Highlight
  ( highlightSpec,
    style,
  )
where

import Clay (Css, (?))
import qualified Clay as C
import Commonmark.Html (Html, htmlInline)
import Commonmark.Inlines (FormattingSpec (FormattingSpec))
import Commonmark.Pandoc (Cm)
import Commonmark.SourceMap (WithSourceMap, addName)
import Commonmark.Syntax (SyntaxSpec (syntaxFormattingSpecs))
import Commonmark.Types (IsBlock, IsInline)
import qualified Text.Pandoc.Builder as B

highlightSpec ::
  (Monad m, IsBlock il bl, IsInline il, HasHighlight il) =>
  SyntaxSpec m il bl
highlightSpec =
  mempty
    { syntaxFormattingSpecs =
        [ FormattingSpec '=' True True Nothing (Just highlight) '='
        ]
    }

class HasHighlight a where
  highlight :: a -> a

instance HasHighlight (Html a) where
  highlight x = htmlInline "mark" (Just x)

instance
  (HasHighlight i, Monoid i) =>
  HasHighlight (WithSourceMap i)
  where
  highlight x = (highlight <$> x) <* addName "highlight"

instance HasHighlight (Cm a B.Inlines) where
  highlight ils =
    B.spanWith attr <$> ils
    where
      attr = ("", ["highlight"], [])

style :: Css
style = do
  -- In lieu of <mark>
  ".highlight" ? do
    C.backgroundColor C.yellow