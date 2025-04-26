module Ty where

import Control.Monad.State
import Data.Text (Text)
import Text.Megaparsec (Pos, unPos)

import Com

type Named a = (Text, a)

{--
 -
 - PARSER TYPES
 -
 --}

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

pLn :: P -> Int
pLn (P p _) = unPos p

-- extract an int tuple from a P
pExt :: P -> (Int, Int)
pExt (P ln col) = (unPos ln, unPos col)

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

{--
 -
 - TYCH TYPES
 --}

type Ctx = [(Text, Type)]

mkCtx :: Ctx
mkCtx = []

{--
 -
 - IR TYPES
 -
 --}

-- "three" address code instrs for ir
-- sometimes contains type information for sized ops
data Instr = Push Type Loc
           | Pop Type Loc
           -- store a literal ir value in location x
           | Lit Type Loc IrVal
           -- create a local variable with name x
           | Local Type Text
           -- push fn param with type T and location x
           | Param Type Loc
           -- call fn x with pushed params
           | Call Type Loc
           -- return the value at loc
           | Ret Loc
           deriving (Show, Eq)

-- global var in module
type Var = (Text, IrVal)

-- module function is a body of instructions from x..y
type Body = (Int, Int)

-- a value
data IrVal = IrInt Type Int
           | IrFun Type Int
           deriving (Show, Eq)

-- a module builder
data Mod = Mod { src :: Text
               -- type context
               , ctx :: Ctx
               -- global vars
               , vars :: [Var]
               -- global functions
               , funs :: [Named Function]
               }
         deriving (Show, Eq)

-- a function builder
data Function = Function { fsrc :: Text
                         , finstrs :: [Instr]
                         , fctx :: Ctx
                         , flocals :: [Named Type]
                         , ftmp :: Int
                         , fmod :: Mod
                         }
              deriving (Show, Eq)

data Loc = Var Int
         | Body Int
         deriving (Show, Eq)

type IrMonad e s a = StateT s (Either e) a

type IrModT a = IrMonad Text Mod a

type IrFunT a = IrMonad Text Function a

runIr :: IrModT a -> Mod -> Res (a, Mod)
runIr ir m = runStateT ir m

mkMod t = Mod { src = t
              , ctx = mkCtx
              , vars = []
              , funs = []
              }

mkFun t m = Function { fsrc = t
                     , finstrs = []
                     , fctx = ctx m
                     , flocals = []
                     , ftmp = 0
                     , fmod = m
                     }

getFromMod :: (Mod -> a) -> IrModT a
getFromMod f = state $ \m -> (f m, m)

getFromFun :: (Function -> a) -> IrFunT a
getFromFun f = state $ \m -> (f m, m)
