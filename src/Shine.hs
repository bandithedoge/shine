{-# LANGUAGE OverloadedStrings #-}

module Shine (shineMain) where

import Shine.Render
import Shine.Types

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.OptStream
import qualified System.Console.Terminal.Size as S
import Text.Pandoc

getAst :: String -> IO Pandoc
getAst path = do
  contents <- readFile path
  runIOorExplode $ readCommonMark def{readerExtensions = getAllExtensions "markdown"} $ T.pack contents

blocksFromAst :: Pandoc -> [Block]
blocksFromAst (Pandoc _ xs) = xs

shineMain :: IO ()
shineMain = do
  opts <- parseArgsWithHelp optionsP
  term <- S.size
  ast <- getAst $ optPath opts

  let shine =
        Shine
          { shWidth = S.width $ fromJust term
          , shBlocks = blocksFromAst ast
          , shOptions = opts
          }

  TIO.putStr $ renderDoc shine
