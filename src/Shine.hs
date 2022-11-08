module Shine (shineMain) where

import Shine.Render
import Shine.Types

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.OptStream
import qualified System.Console.Terminal.Size as S
import Text.Pandoc.Definition

shineMain :: IO ()
shineMain = do
  opts <- parseArgsWithHelp optionsP
  term <- S.size
  doc <- BS.getContents

  let shine =
        Shine
          { shWidth = S.width $ fromJust term
          , shDoc = fromJust $ decode doc
          , shOptions = opts
          }

  TIO.putStr $ renderDoc shine
