module Shine (shineMain) where

import Shine.Render
import Shine.Types

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Options.OptStream
import qualified System.Console.Terminal.Size as S
import System.Process.ByteString.Lazy
import Text.Pandoc.Definition

readDoc :: FilePath -> IO Pandoc
readDoc path = do
  content <-
    if path == ""
      then BS.getContents
      else do
        stdin' <- BS.readFile path
        (code, stdout', stderr') <- readProcessWithExitCode "pandoc" ["--to", "json"] stdin'
        return stdout'
  return $ fromJust $ decode content

shineMain :: IO ()
shineMain = do
  opts <- parseArgsWithHelp optionsP
  term <- S.size
  doc <- readDoc $ optPath opts

  let shine =
        Shine
          { shWidth = S.width $ fromJust term
          , shDoc = doc
          , shOptions = opts
          }

  TIO.putStr $ renderDoc shine
