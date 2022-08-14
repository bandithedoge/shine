module Shine.Types (
  Shine (..),
  defaultShine,
  Options (..),
  optionsP,
  ShineException (..),
) where

import Control.Applicative
import Control.Exception (Exception)
import Data.Functor
import Options.OptStream
import Text.Pandoc
import Text.Pandoc.Definition

data Shine = Shine
  { shWidth :: Int
  , shBlocks :: [Block]
  , shOptions :: Options
  }

defaultShine :: Shine
defaultShine =
  Shine
    { shWidth = 0
    , shBlocks = []
    , shOptions =
        Options
          { optPath = ""
          , optFormat = ""
          , optStrict = False
          }
    }

data Options = Options
  { optPath :: String
  , optFormat :: String
  , optStrict :: Bool
  }

optionsP :: Parser Options
optionsP =
  Options
    <$> freeArg "ARG" "Path"
    <#> (param ["-f", "--format"] "STR" "Specify input format manually." <|> orElse "")
    <#> (flag ["-s", "--strict"] "Strict mode (error on unhandled syntax, only useful for debugging)" $> True <|> orElse False)

data ShineException = StrictMode deriving (Show)

instance Exception ShineException
