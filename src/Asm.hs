module Asm where

import Data.Text
import Text.Megaparsec
import Text.Printf

import Com
import Parse

data Asm = Asm { dataSect :: [String]
               , instrs :: [String]
               }
         deriving (Show, Eq)

newAsm = Asm { dataSect = [], instrs = [] }

asmNewData :: Asm -> String -> Asm
asmNewData a x = Asm { dataSect = x:dataSect a, instrs = instrs a }

-- apply f to each item in list carrying the asm over each op
-- foldStmts :: Asm -> (Asm -> Stmt -> Res Asm) -> [Stmt] -> Res Asm
-- foldStmts a _ [] = Right a
-- foldStmts a f (s:ss) = do{ a' <- f a s
--                          ; foldStmts a' f ss
--                          }

-- compileStmt :: Asm -> Stmt -> Res Asm
-- compileStmt asm x = Left $ printf "cannot compile statement %s" $ show x

-- compile :: Text -> Either String Asm
-- compile x = do{ s <- case parse stmts "" x of
--                          Left e -> Left $ errorBundlePretty e
--                          Right x -> Right x
--               ; x <- foldStmts newAsm compileStmt s
--               ; return x
--               }
