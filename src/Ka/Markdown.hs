module Ka.Markdown
  ( noteExtension,
    notePattern,
    noteFileTitle,
    parseMarkdown,
    queryLinks,
    getNoteLink,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Pandoc as CP
import qualified Data.Set as Set
import qualified Data.Text as T
import Ka.Plugin (CMSyntaxSpec)
import System.FilePath (dropExtension)
import System.FilePattern (FilePattern)
import qualified Text.Pandoc.Builder as B
import Text.Pandoc.Definition (Attr, Inline (Link), Pandoc (..), Target)
import qualified Text.Pandoc.Walk as W

noteExtension :: String
noteExtension = ".md"

-- | Pattern of filenames to treat as notes
notePattern :: FilePattern
notePattern = "*" <> noteExtension

noteFileTitle :: FilePath -> Text
noteFileTitle = toText . dropExtension

parseMarkdown :: CMSyntaxSpec -> FilePath -> Text -> Pandoc
parseMarkdown spec fp s =
  either (error . show) toPandoc $ join $ CM.commonmarkWith spec fp s
  where
    toPandoc = Pandoc mempty . B.toList . CP.unCm

queryLinks :: Pandoc -> Set FilePath
queryLinks = Set.fromList . W.query go
  where
    go :: Inline -> [FilePath]
    go = maybeToList . getNoteLinkFilePath

-- | Get the note filename from its link
getNoteLinkFilePath :: Inline -> Maybe FilePath
getNoteLinkFilePath x = do
  (_attr, _inlines, (url, _title)) <- getNoteLink x
  pure $ toString url

getNoteLink :: Inline -> Maybe (Attr, [Inline], Target)
getNoteLink = \case
  Link attr inlines target@(url, _title) -> do
    guard $ not $ "/" `T.isInfixOf` url
    guard $ ".md" `T.isSuffixOf` url
    pure (attr, inlines, target)
  _ ->
    Nothing
