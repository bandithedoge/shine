module Shine.Types (
  Shine (Shine, width, blocks),
  defaultShine,
) where

import Text.Pandoc
import Text.Pandoc.Definition

data Shine = Shine {width :: Int, blocks :: [Block]}

defaultShine :: Shine
defaultShine = Shine{width = 80, blocks = []}
