module Ka.Markdown
  ( noteExtension,
    mdFileThing,
    parseMarkdown,
    queryNoteLinksWithContext,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Ka.Graph (ThingName (..))
import System.FilePath (dropExtension)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Block, Pandoc (..))
import qualified Text.Pandoc.LinkContext as LC

type CMSyntaxSpec =
  CM.SyntaxSpec
    (Either CM.ParseError)
    (CP.Cm () B.Inlines)
    (CP.Cm () B.Blocks)

noteExtension :: String
noteExtension = ".md"

mdFileThing :: FilePath -> ThingName
mdFileThing = ThingName . toText . dropExtension

parseMarkdown :: CMSyntaxSpec -> FilePath -> Text -> Pandoc
parseMarkdown spec fp s =
  either (error . show) toPandoc $ join $ CM.commonmarkWith spec fp s
  where
    toPandoc = Pandoc mempty . B.toList . CP.unCm

queryNoteLinksWithContext :: Pandoc -> Map FilePath [Block]
queryNoteLinksWithContext =
  Map.mapKeys toString
    . Map.filterWithKey (flip $ const isNoteUrl)
    . LC.queryLinksWithContext
  where
    isNoteUrl :: Text -> Bool
    isNoteUrl url =
      not ("/" `T.isInfixOf` url) && (toText noteExtension `T.isSuffixOf` url)
