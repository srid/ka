module Ka.Plugin.Tag
  ( inlineTagSpec,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import Commonmark.TokParsers (noneOfToks, symbol)
import Commonmark.Tokens (TokType (..))
import qualified Text.Parsec as P

inlineTagSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
inlineTagSpec =
  mempty
    { CM.syntaxInlineParsers = [pInlineTag]
    }
  where
    pInlineTag ::
      (Monad m, CM.IsInline il) =>
      CM.InlineParser m il
    pInlineTag = P.try $ do
      _ <- symbol '#'
      tag <- CM.untokenize <$> inlineTagP
      pure $ CM.code tag

inlineTagP :: Monad m => P.ParsecT [CM.Tok] s m [CM.Tok]
inlineTagP =
  some (noneOfToks $ [Spaces, UnicodeSpace, LineEnd] <> fmap Symbol punctuation)
  where
    punctuation = "[];:,.?!"