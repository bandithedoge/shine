-- https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797
module Shine.ANSI (
  formatWith,
) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import Numeric
import Text.Read (readMaybe)

colorMap, styleMap :: HM.HashMap String Int
colorMap =
  HM.fromList
    [ ("black", 0)
    , ("red", 1)
    , ("green", 2)
    , ("yellow", 3)
    , ("blue", 4)
    , ("magenta", 5)
    , ("cyan", 6)
    , ("white", 7)
    , ("brightBlack", 8)
    , ("brightRed", 9)
    , ("brightGreen", 10)
    , ("brightYellow", 11)
    , ("brightBlue", 12)
    , ("brightMagenta", 13)
    , ("brightCyan", 14)
    , ("brightWhite", 15)
    ]
styleMap =
  HM.fromList
    [ ("bold", 1)
    , ("dim", 2)
    , ("italic", 3)
    , ("underline", 4)
    , ("strikeout", 9)
    ]

defaultEscape, defaultReset :: String
defaultEscape = "\x1b["
defaultReset = "0m"

data Code = Code
  { escape :: String
  , start :: String
  , reset :: String
  }
  deriving (Show)

defaultCode :: Code
defaultCode =
  Code
    { escape = defaultEscape
    , start = defaultReset
    , reset = defaultReset
    }

hexToRGB :: String -> [String]
hexToRGB x = [r, g, b]
 where
  hex = map T.unpack $ T.chunksOf 2 (T.pack $ L.tail x)
  convert x = show $ fst $ L.head $ readHex x
  r = convert $ L.head hex
  g = convert $ hex L.!! 1
  b = convert $ hex L.!! 2

style :: String -> Code
-- for whatever dumb reason `22m` resets both bold and dim
-- `21m` activates double underline instead of resetting bold
style "bold" = defaultCode{start = "1m", reset = "22m"}
style x
  | isJust (readMaybe base :: Maybe Int) =
    defaultCode
      { start = (if isBg then "48;5;" else "38;5;") ++ base ++ "m"
      }
  | L.head x == '#' =
    let rgb = hexToRGB base
     in defaultCode
          { start =
              ( if isBg
                  then "48;2;"
                  else "38;2;"
              )
                ++ L.head rgb
                ++ ";"
                ++ rgb L.!! 1
                ++ ";"
                ++ rgb L.!! 2
                ++ "m"
          }
  | HM.member base colorMap = style $ show (colorMap HM.! (if isBg then L.init x else x)) ++ (if isBg then "!" else "")
  | HM.member base styleMap =
    defaultCode
      { start = show (styleMap HM.! base) ++ "m"
      , reset = "2" ++ show (styleMap HM.! base) ++ "m"
      }
  | otherwise = defaultCode{escape = "", start = "", reset = ""}
 where
  isBg = L.last x == '!'
  base = if isBg then L.init x else x

wrap :: Code -> T.Text -> T.Text
wrap x txt = T.pack (escape x ++ start x) <> txt <> T.pack (escape x ++ reset x)

formatWith :: [String] -> T.Text -> T.Text
formatWith xs txt = starts <> txt <> resets
 where
  codes = map style xs
  starts = T.pack $ concatMap (\x -> escape x ++ start x) codes
  resets = T.pack $ concatMap (\x -> escape x ++ reset x) codes
