{-# LANGUAGE CPP #-}

module Parse where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void

import Com
import Ty
import Fmt

data ParseErr = PrsErrTxt Text
                deriving (Show, Eq, Ord)

instance ShowErrorComponent ParseErr where
    showErrorComponent (PrsErrTxt t) = T.unpack t
    errorComponentLen (PrsErrTxt t) = T.length t

type Parser = Parsec ParseErr Text

-- turn some literal text into a simple type
txtToType :: Text -> Parser Type
txtToType "u8" = pure $ Unsigned 8
txtToType "u16" = pure $ Unsigned 16
txtToType "u32" = pure $ Unsigned 32
txtToType "u64" = pure $ Unsigned 64
txtToType "i8" = pure $ Signed 8
txtToType "i16" = pure $ Signed 16
txtToType "i32" = pure $ Signed 32
txtToType "i64" = pure $ Signed 64
txtToType x = customFailure $ PrsErrTxt $ T.pack $ printf "invalid type %s" x

getPos :: Parser P
getPos = do{ pos <- getSourcePos
           ; return $ P (sourceLine pos) (sourceColumn pos)
           }

spaces :: Parser ()
spaces = do { _ <- many space1
            ; pure ()
            } <?> "spaces"

int :: Parser Leaf
int = do{ dbgTrace "int"
        ; p <- getPos
        ; h <- digitChar
        ; i <- many digitChar
        ; return $ Leaf p $ I $ read $ printf "%c%s" h i
        } <?> "integer"

flt :: Parser Leaf
flt = do{ dbgTrace "flt"
        ; p <- getPos
        ; h <- digitChar
        ; i' <- manyTill digitChar $ char '.' -- int
        ; let i = [h] ++ i'
        ; d <- many digitChar -- decimal 
        ; let i = if d == "" then i else printf "%s.%s" i d
        ; return $ Leaf p $ F $ read i
        } <?> "float"

num :: Parser Leaf
num = (try flt <|> int) <?> "number"

str :: Parser Leaf
str = do{ dbgTrace "str"
        ; p <- getPos
        ; _ <- char '"'
        ; s <- (many $ anySingleBut '"') <?> "string"
        ; return $ Leaf p $ Str $ T.pack s
        } <?> "string"

verbStr :: Parser Text
verbStr = do{ h <- v
            ; t <- many v
            ; return $ T.pack $ h:t
            } <?> "verb string"
            where
                vs :: String
                vs = "~!@#$%^&*_+-=|:'<>?/.,\\|"
                v :: Parser Char
                v = oneOf vs

monad :: Parser Leaf
monad = do{ dbgTrace "monad"
          ; p <- getPos
          ; v <- verbStr
          ; x <- expr
          ; return $ Leaf p $ V v [x]
          } <?> "monad"

dyad :: Parser Leaf
dyad = do{ dbgTrace "dyad"
         ; p <- getPos
         ; x <- try term <|> noun
         ; spaces
         ; v <- verbStr
         ; y <- expr
         ; return $ Leaf p $ V v [x, y]
         } <?> "dyad"

mexpr :: Parser Leaf
mexpr = do{ dbgTrace "mexpr"
          ; p <- getPos
          ; x <- noun
          ; spaces
          ; char '['
          ; y <- exprs
          ; char ']'
          ; return $ Leaf p $ M x y
          }

-- an arg like `name: type`
arg :: Parser (Leaf, Type)
arg = do{ dbgTrace "arg"
        ; n <- name
        ; spaces
        ; char ':'
        ; spaces
        ; t <- typ'
        ; return (n, t)
        } <?> "function argument"

sig :: Parser Sig
sig = do{ dbgTrace "sig"
        ; a <- args
        ; r <- typ'
        ; let a' = case a of
                       Left _ -> []
                       Right a -> a
          in return $ Sig r a'
        } <?> "function signature"
        where
            sep = do{ spaces
                    ; string "->"
                    ; spaces
                    }
            args' = do{ dbgTrace "sig.args'"
                      ; as <- try arg `sepBy` try arrow
                      ; sep
                      ; return as
                      }
            args = observing args'
            arrow = try $ do{ dbgTrace "sig.arrow"
                            ; sep
                            ; notFollowedBy typ'
                            }

fun :: Parser Leaf
fun = do{ dbgTrace "fun"
        ; p <- getPos
        ; char '{'
        ; spaces
        ; s <- sig
        ; spaces
        ; char ';'
        ; spaces
        ; x <- exprs
        ; spaces
        ; char '}'
        ; return $ Leaf p $ O s x
        } <?> "function"

noun :: Parser Leaf
noun = do{ dbgTrace "noun"
         ; (name <|> num <|> str <|> fun) <?> "noun"
         }

name' :: Parser Text
name' = do{ dbgTrace "name'"
          ; h <- letterChar
          ; t <- (many $ alphaNumChar <|> char '_') <?> "valid name chars"
          ; return $ T.pack $ h:t
          } <?> "name'"

name :: Parser Leaf
name = do{ dbgTrace "name"
         ; p <- getPos
         ; x <- name'
         ; return $ Leaf p $ X x
         } <?> "name"

typ' :: Parser Type
typ' = do{ dbgTrace "typ'"
         ; n <- name'
         ; txtToType n
         } <?> "typename'"

typ :: Parser Leaf
typ = do{ dbgTrace "typ"
        ; p <- getPos
        ; t <- typ'
        ; return $ Leaf p $ T t
        } <?> "typename"

let_ :: Parser ()
let_ = do{ _ <- string $ T.pack "let"
         ; pure ()
         }

bind3 :: Parser Leaf
bind3 = do{ dbgTrace "bind3"
          ; p <- getPos
          ; let_
          ; spaces
          ; n <- name'
          ; spaces
          ; _ <- char ':'
          ; spaces
          ; t <- typ
          ; spaces
          ; _ <- char '='
          ; spaces
          ; x <- expr
          ; return $ Leaf p $ V "let" [Leaf p $ X n, t, x]
          } <?> "ternary let binding"

bind2 :: Parser Leaf
bind2 = do{ dbgTrace "bind2"
          ; p <- getPos
          ; let_
          ; spaces
          ; n <- name
          ; spaces
          ; _ <- char '='
          ; spaces
          ; x <- expr
          ; return $ Leaf p $ V "let" [n, x]
          } <?> "binary let binding"

bind :: Parser Leaf
bind = try bind3 <|> bind2

term :: Parser Leaf
term = do{ dbgTrace "term"
         ; mexpr <|> noun
         }

expr :: Parser Leaf
expr = do{ dbgTrace "expr"
         ; spaces
         ; x <- choice e <?> "expression"
         ; spaces
         ; return x
         } <?> "expression"
         where
             e = [ try bind
                 , try dyad
                 , monad
                 , try term
                 , noun
                 ]

exprs :: Parser [Leaf]
exprs = do{ dbgTrace "exprs"
          ; x <- expr
          ; y <- observing rest
          ; return $ case y of
                         Left _ -> [x]
                         Right tail -> x:tail
          } <?> "expressions"
          where
              rest :: Parser [Leaf]
              rest = do{ char ';'
                       ; spaces
                       ; exprs
                       }

top :: Parser [Leaf]
top = do{ e <- exprs
        ; eof
        ; return e
        }

prs :: Text -> IO ()
prs x = putStrLn $ case parse exprs "" x of
                       Left e -> errorBundlePretty e
                       Right x -> show $ map fmt x
