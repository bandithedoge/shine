module Shine.Types (
  Shine (Shine, shWidth, shBlocks),
  defaultShine,
  Options (Options, optPath, optFormat),
  optionsP,
) where

import Control.Applicative
import Options.OptStream
import Text.Pandoc
import Text.Pandoc.Definition

data Shine = Shine
  { shWidth :: Int
  , shBlocks :: [Block]
  }

defaultShine :: Shine
defaultShine = Shine{shWidth = 80, shBlocks = []}

data Options = Options
  { optPath :: String
  , optFormat :: String
  }

optionsP :: Parser Options
optionsP =
  Options
    <$> freeArg "ARG" "Path"
    <#> (param ["-f", "--format"] "STR" "Specify input format manually." <|> orElse "")
