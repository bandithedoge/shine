module Shine.Render (
  renderInline,
  renderBlock,
  renderDoc,
) where

import Shine.ANSI
import Shine.Types
import Shine.Util

import Data.Char (isUpper, toUpper)
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T

import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Wrap

renderDelimiter :: ListNumberDelim -> T.Text
renderDelimiter Period = T.singleton '.'
renderDelimiter OneParen = T.singleton ')'
renderDelimiter TwoParens = T.replicate 2 $ T.singleton ')'
renderDelimiter DefaultDelim = renderDelimiter Period

renderQuote :: QuoteType -> T.Text
renderQuote SingleQuote = T.singleton '\''
renderQuote DoubleQuote = T.singleton '"'

renderInlines :: [Inline] -> T.Text
renderInlines xs = T.concat $ map (renderInline defaultShine) xs

-- https://hackage.haskell.org/package/pandoc-types-1.22.2/docs/Text-Pandoc-Definition.html#t:Inline
renderInline :: Shine -> Inline -> T.Text
renderInline _ (Str x) = x
renderInline _ (Emph xs) = formatWith ["italic"] $ renderInlines xs
renderInline _ (Underline xs) = formatWith ["underline"] $ renderInlines xs
renderInline _ (Strong xs) = formatWith ["bold"] $ renderInlines xs
renderInline _ (Strikeout xs) = formatWith ["strikeout"] $ renderInlines xs
renderInline _ (Superscript xs) = formatWith ["bold", "italic"] $ renderInlines xs
renderInline _ (Subscript xs) = formatWith ["underline", "italic"] $ renderInlines xs
renderInline _ (Quoted x xs) =
  formatWith ["italic"] $
    renderQuote x
      <> renderInlines xs
      <> renderQuote x
renderInline _ (Code _ x) = formatWith ["dim"] x
renderInline _ Space = T.singleton ' '
renderInline _ SoftBreak = T.singleton '\n'
renderInline _ LineBreak = T.singleton '\n'
renderInline _ (RawInline _ x) = formatWith ["dim"] x
renderInline _ (Link attr xs x) =
  formatWith ["cyan"] (renderInlines xs)
    <> formatWith
      ["dim"]
      ( T.pack " ("
          <> fst x
          <> T.pack ")"
      )
renderInline _ (Image attr xs x) =
  formatWith ["blue"] (T.singleton '[' <> renderInlines xs <> T.singleton ']')
    <> formatWith
      ["dim"]
      ( T.pack " ("
          <> fst x
          <> T.pack ")"
      )
renderInline _ x = T.pack $ show x

renderBlocks :: Shine -> [Block] -> T.Text
renderBlocks shine xs =
  T.intercalate (T.singleton '\n') $
    map (renderBlock shine) xs

-- https://hackage.haskell.org/package/pandoc-types-1.22.2/docs/Text-Pandoc-Definition.html#t:Block
renderBlock :: Shine -> Block -> T.Text
renderBlock _ (Plain xs) = renderInlines xs
renderBlock _ (Para xs) = renderInlines xs
renderBlock _ (LineBlock xs) = T.concat $ map renderInlines xs
renderBlock shine (CodeBlock attr x) =
  box
    (width shine)
    ( let langs = (\(_, a, _) -> a) attr
       in ( if L.null langs
              then T.empty
              else
                formatWith
                  ["dim"]
                  (L.head langs)
          )
            <> T.singleton '\n'
            <> x
    )
renderBlock shine (RawBlock _ xs) = indent $ formatWith ["dim"] xs
renderBlock shine (BlockQuote xs) = indent $ formatWith ["italic"] $ renderBlocks shine xs
renderBlock shine (OrderedList x xs) =
  T.intercalate (T.singleton '\n') $
    map
      ( \y ->
          T.pack
            ( show $
                fromJust (L.elemIndex y xs)
                  + (\(a, _, _) -> a) x
            )
            <> renderDelimiter ((\(_, _, a) -> a) x)
            <> T.singleton ' '
            <> renderBlocks shine y
      )
      xs
renderBlock shine (BulletList xs) =
  T.intercalate (T.singleton '\n') $
    map (\xs -> T.singleton '\x2022' <> T.singleton ' ' <> renderBlocks shine xs) xs
renderBlock _ (Header i _ xs) =
  formatWith ["black", "brightWhite!", "bold"] $
    T.concat
      [ T.replicate i $ T.singleton '#'
      , T.singleton ' '
      , renderInlines xs
      ]
renderBlock shine HorizontalRule = T.pack $ replicate (width shine) '\x2501'
renderBlock _ xs = T.pack $ show xs

renderDoc :: Shine -> T.Text
renderDoc shine =
  wrap (width shine) $
    T.intercalate (T.pack "\n\n") $ map (renderBlock shine) $ blocks shine
