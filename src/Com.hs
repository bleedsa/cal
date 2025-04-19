module Com where

import Text.Megaparsec

type Res t = Either String t

-- line, col position
data P = P Pos Pos
         deriving (Show, Eq)
