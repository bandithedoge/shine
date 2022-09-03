module Shine.Util (
  indent,
  box,
  wrap,
  rule,
) where

import Shine.ANSI
import Shine.Types

import qualified Data.HashMap.Lazy as HM
import Data.Hashable
import qualified Data.Text as T

import Hable.BoxChar
import Text.Pandoc.Definition
import Text.Wrap

indent :: T.Text -> T.Text
indent txt = T.replicate 4 (T.pack " ") <> txt

wrap :: Int -> T.Text -> T.Text
wrap i txt =
  if i == 0
    then txt
    else
      wrapText
        defaultWrapSettings
          { preserveIndentation = True
          , breakLongWords = True
          }
        i
        txt

box :: Int -> T.Text -> T.Text
box width txt = T.intercalate (T.singleton '\n') $ map indent $ T.lines txt

rule :: Shine -> T.Text
rule shine = T.pack $ replicate (shWidth shine) '\x2501'
