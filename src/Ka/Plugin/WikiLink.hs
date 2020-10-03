module Ka.Plugin.WikiLink
  ( wikiLinkSpec,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import Commonmark.TokParsers (noneOfToks, symbol)
import Commonmark.Tokens (TokType (LineEnd, Symbol))
import Ka.Markdown (noteExtension)
import qualified Text.Parsec as P

wikiLinkSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
wikiLinkSpec =
  wikiLinkSpec' (<> toText noteExtension)

wikiLinkSpec' ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  -- | Transform the wikilink inner text to a working URL
  --
  -- Eg: @(<> ".html")
  (Text -> Text) ->
  CM.SyntaxSpec m il bl
wikiLinkSpec' f =
  mempty
    { CM.syntaxInlineParsers = [pLink]
    }
  where
    pLink :: (Monad m, CM.IsInline il) => CM.InlineParser m il
    pLink = P.try $ do
      s <- wikiLinkP
      let url = f s
          title = ""
      pure $ CM.link url title (CM.str s)
    wikiLinkP :: Monad m => P.ParsecT [CM.Tok] s m Text
    wikiLinkP = do
      let allowedChars = noneOfToks [Symbol ']', LineEnd]
      void $ P.count 2 $ symbol '['
      s <- fmap CM.untokenize $ some allowedChars
      void $ P.count 2 $ symbol ']'
      pure s
