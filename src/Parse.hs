module Parse where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf
import Data.Text
import Data.Void

type Parser = Parsec Void Text

data S = I Int
       | F Double
       | Str Text
       | Cons Text [S]
       deriving (Show, Eq)

spaces :: Parser ()
spaces = do { _ <- many space1
            ; pure ()
            }

int :: Parser S
int = do{ h <- digitChar
        ; i <- many digitChar
        ; return $ I $ read $ printf "%c%s" h i
        } <?> "integer"

flt :: Parser S
flt = do{ h <- digitChar
        ; i' <- manyTill digitChar $ char '.' -- int
        ; let i = [h] ++ i'
        ; d <- many digitChar -- decimal 
        ; return $ F $ read $ if d == ""
                              then i
                              else printf "%s.%s" i d
        }

num :: Parser S
num = (try flt <|> int) <?> "number"

str :: Parser S
str = do{ _ <- char '"'
        ; s <- (many $ anySingleBut '"') <?> "string"
        ; return $ Str $ pack s
        } <?> "string"

verbStr :: Parser Text
verbStr = do{ h <- v
            ; t <- many v
            ; return $ pack $ [h] ++ t
            }
            where
                vs :: String
                vs = "~!@#$%^&*_+-=|:'<>?/.,\\|"
                v :: Parser Char
                v = oneOf vs

monad :: Parser S
monad = do{ v <- verbStr
          ; x <- expr
          ; return $ Cons v [x]
          }

dyad :: Parser S
dyad = do{ x <- noun
         ; spaces
         ; v <- verbStr
         ; y <- expr
         ; return $ Cons v [x, y]
         }

noun :: Parser S
noun = (num <|> str) <?> "noun"

expr :: Parser S
expr = do{ spaces
         ; x <- (monad <|> try dyad <|> noun)
         ; spaces
         ; return x
         }

run :: Text -> IO ()
run x = parseTest expr x
