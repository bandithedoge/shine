module Shine (
  shineMain,
) where

import Shine.Render
import Shine.Types

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Console.Terminal.Size as S
import Text.Pandoc
import Text.Wrap

mdToAst :: String -> IO Pandoc
mdToAst txt = runIOorExplode $ readCommonMark def $ T.pack txt

blocksFromAst :: Pandoc -> [Block]
blocksFromAst (Pandoc _ xs) = xs

shineMain :: IO ()
shineMain = do
  text <- readFile "test.md"
  ast <- mdToAst text
  term <- S.size
  TIO.putStr $ renderDoc Shine{width = S.width $ fromJust term, blocks = blocksFromAst ast}
