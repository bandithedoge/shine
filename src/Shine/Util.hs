module Shine.Util (
  indent,
  extractSecond,
  box,
  wrap,
) where

import Shine.ANSI

import qualified Data.Text as T

import Text.Wrap

indent :: T.Text -> T.Text
indent txt = T.replicate 4 (T.pack " ") <> txt

wrap :: Int -> T.Text -> T.Text
wrap = wrapText defaultWrapSettings{preserveIndentation = True}

box :: Int -> T.Text -> T.Text
box width txt = T.intercalate (T.singleton '\n') $ map indent $ T.lines txt

extractSecond :: (a, b, c) -> b
extractSecond (_, x, _) = x
