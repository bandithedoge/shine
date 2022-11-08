{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
import qualified Data.HashMap.Lazy as HM
import qualified Data.Map.Lazy as M
import Options.OptStream
import Text.Pandoc.Definition

data Shine = Shine
  { shWidth :: Int
  , shDoc :: Pandoc
  , shOptions :: Options
  }

defaultShine :: Shine
defaultShine =
  Shine
    { shWidth = 0
    , shDoc = Pandoc (Meta M.empty) []
    , shOptions =
        Options
          { optStrict = False
          }
    }

data Options = Options
  { optStrict :: Bool
  }

optionsP :: Parser Options
optionsP =
  Options
    <$> (flag ["-s", "--strict"] "Strict mode (error on unhandled syntax, only useful for debugging)" $> True <|> orElse False)

data ShineException = StrictMode deriving (Show)

instance Exception ShineException
