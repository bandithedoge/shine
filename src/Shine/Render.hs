module Shine.Render (
  renderInline,
  renderBlock,
  renderDoc,
) where

import Shine.ANSI
import Shine.Types
import Shine.Util

import Control.Exception
import Data.Char (isUpper, toUpper)
import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T

import Hable
import Hable.BoxChar
import Hable.Config
import Hable.Style.Unicode
import Text.Pandoc.Definition
import Text.Pandoc.Walk
import Text.Wrap

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
renderInline _ (SmallCaps xs) =
  T.concat $
    map
      ( \x ->
          if isUpper x
            then formatWith ["bold"] $ T.singleton x
            else formatWith ["dim"] $ T.singleton x
      )
      $ T.unpack (renderInlines xs)
renderInline _ (Quoted x xs) =
  formatWith ["italic"] $
    renderQuote x
      <> renderInlines xs
      <> renderQuote x
  where
    renderQuote SingleQuote = T.singleton '\''
    renderQuote DoubleQuote = T.singleton '"'
renderInline _ (Code _ x) = formatWith ["dim"] x
renderInline _ Space = T.singleton ' '
renderInline _ SoftBreak = T.singleton '\n'
renderInline _ LineBreak = T.singleton '\n'
renderInline _ (RawInline _ x) = formatWith ["dim"] x
renderInline _ (Math _ x) = formatWith ["green"] x
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
renderInline _ (Span _ xs) = renderInlines xs
renderInline shine xs =
  if optStrict $ shOptions shine
    then throw StrictMode
    else T.pack $ show xs

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
    (shWidth shine)
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
  where
    renderDelimiter Period = T.singleton '.'
    renderDelimiter OneParen = T.singleton ')'
    renderDelimiter TwoParens = T.replicate 2 $ T.singleton ')'
    renderDelimiter DefaultDelim = renderDelimiter Period
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
renderBlock shine HorizontalRule = T.pack $ replicate (shWidth shine) '\x2501'
renderBlock shine (Table attr cap specs head bodies foot) =
  T.pack $
    hable
      defaultConfig
        { hLineStyle =
            if L.null $ rowsFromHead head
              then normal
              else thickBorder
        , vLineStyle = normal
        }
      ( ( let cells = map cellsFromRow $ rowsFromHead head
              blocks = concatMap (map blocksFromCell) cells
           in map renderCell blocks
        ) :
        let rows = concatMap rowsFromBody bodies
            cells = map cellsFromRow rows
         in map (map (renderCell . blocksFromCell)) cells
      )
  where
    normal m n = if n `elem` [1, m] then Just Thick else Just Normal
    thickBorder m n = if n `elem` [1, 2, m] then Just Thick else Just Normal
    rowsFromHead (TableHead _ xs) = xs
    rowsFromBody (TableBody _ _ xs ys) = ys
    cellsFromRow (Row _ xs) = xs
    blocksFromCell (Cell _ _ _ _ xs) = xs
    renderCell x = T.unpack $ wrapCell $ renderBlocks shineConfig x
    shineConfig = shine{shWidth = 0}
    wrapCell = wrap $ (shWidth shine `div` length specs) - 4
renderBlock shine (Div _ xs) = renderBlocks shine xs
renderBlock shine Null = T.empty
renderBlock shine xs =
  if optStrict $ shOptions shine
    then throw StrictMode
    else T.pack $ show xs

renderDoc :: Shine -> T.Text
renderDoc shine =
  wrap (shWidth shine) $
    T.intercalate (T.pack "\n\n") $ map (renderBlock shine) $ shBlocks shine
