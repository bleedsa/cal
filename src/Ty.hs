module Ty where

import Data.Text (Text)
import Text.Megaparsec (Pos)

import Com

-- line, col position
data P = P Pos Pos
         deriving (Show, Eq)

-- a variable type
data Type = GenInt
          | Signed Int      -- x = number of bits
          | Unsigned Int    -- x = number of bits
          | Float Int       -- x = number of bits
          | Chr
          | Void
          | Slice Type
          | Fun Sig
          | Name Text
          | Arrow Type Type -- arrow from x -> y
          deriving(Show, Eq)

type Arg = (Leaf, Type)
type Args = [Arg]

-- a function signature.
-- 0: return type
-- 1: argument types
data Sig = Sig Type Args
         deriving(Show, Eq)

-- S expression stems
data S = I Int         -- integer
       | F Double      -- float
       | Str Text      -- string
       | V Text [Leaf] -- verb (Cons)
       | O Sig [Leaf]  -- lambda
       | A [Leaf]      -- vec
       | T Type        -- typename
       | X Text        -- identifier
       deriving (Show, Eq)

-- a leaf is a position and an expression stem
data Leaf = Leaf P S
          deriving (Show, Eq)

leafP :: Leaf -> P
leafP (Leaf p _) = p

argsToType :: Args -> Type
argsToType [] = Void
argsToType [(_, t)] = t
argsToType ((_, h):t) = Arrow h $ argsToType t

typesToArrow :: [Type] -> Type
typesToArrow [] = Void
typesToArrow [t] = t
typesToArrow (h:t) = Arrow h $ typesToArrow t

argType :: Arg -> Type
argType (_, t) = t

sOf :: Leaf -> S
sOf (Leaf _ s) = s
