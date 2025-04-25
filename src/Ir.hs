module Ir where

import Data.Text (Text)
import Control.Monad.State
import Text.Printf
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text as T

import Com
import Parse
import Ty
import TyCh
import Fmt

-- "three" address code instrs
-- sometimes contains type information for sized ops
data Instr = Push Type Loc
           | Pop Type Loc
           | Lit Type Loc IrVal
           | Param Type Loc        -- push fn param with type T and location x
           | Call Type Loc         -- call fn x
           | Ret Loc
           deriving (Show, Eq)

-- var number x in function has type y
type Var = IrVal

-- function is a body at index x
type Fun = (Int, Int)

-- a value
data IrVal = IrInt Type Int
           | IrFun Fun
           deriving (Show, Eq)

-- a module
data Mod = Mod { instrs :: [Instr]
               , ctx :: Ctx
               , vars :: [Var]
               , funs :: [Fun]
               }
         deriving (Show, Eq)

data Loc = Var Int
         | Body Int
         | Fun Int
         deriving (Show, Eq)

type IrMonad e s a = StateT s (Either e) a

type Ir a = IrMonad Text Mod a

runIr :: Ir a -> Mod -> Res (a, Mod)
runIr ir m = runStateT ir m

mkMod = Mod { instrs = []
            , ctx = mkCtx
            , vars = []
            , funs = []
            }

cmpErr' :: P -> String -> Text
cmpErr' p x = T.pack $ printf "err: 'compile: %s\n%s" (fmtP p) x

cmpErr :: P -> String -> Res a
cmpErr p x = Left $ cmpErr' p x

-- unwrap an Either into an Ir err
un :: Either e a -> IrMonad e s a
un (Right x) = pure x
un (Left e) = lift $ Left e

-- unwrap a maybe or return 
maybeOr :: Maybe a -> e -> IrMonad e s a
maybeOr (Just x) _ = pure x
maybeOr Nothing e = lift $ Left e

joinAndIdx :: a -> [a] -> (Int, [a])
joinAndIdx x v = (i, x:v)
               where
                   i = length v

pushCtx' :: (Leaf, Type) -> Mod -> (Int, Mod)
pushCtx' x m = let (i, v) = joinAndIdx x $ ctx m
               in (i, Mod { instrs = instrs m
                          , ctx = x:ctx m
                          , vars = vars m
                          , funs = funs m
                          })

pushInstr' :: Instr -> Mod -> (Int, Mod)
pushInstr' x m = let (i, v) = joinAndIdx x $ instrs m
                 in (i,  Mod { instrs = v
                             , ctx = ctx m
                             , vars = vars m
                             , funs = funs m
                             })

pushVar' :: Var -> Mod -> (Int, Mod)
pushVar' x m = ( i
               , Mod { instrs = instrs m
                     , ctx = ctx m
                     , vars = a
                     , funs = funs m
                     }
               )
               where
                   v = vars m
                   i = length v 
                   a = x:v

pushFun' :: Fun -> Mod -> (Int, Mod)
pushFun' x m = let (i, v) = joinAndIdx x $ funs m
               in (i, Mod { instrs = instrs m
                          , ctx = ctx m
                          , vars = vars m
                          , funs = v
                          })

{--
 - wrap some common ops with State
 --}

-- return from a state update lambda with () and the new state
retNil :: a -> ((), a)
retNil x = ((), x)

-- update a module with a function that takes a Mod and returns a new Mod
updMod :: (Mod -> Mod) -> Ir ()
updMod f = state $ \m -> retNil $ f m

updModThen :: (Mod -> (a, Mod)) -> Ir a
updModThen f = state $ \m -> let (i, m') = f m
                             in (i, m')

pushCtx :: (Leaf, Type) -> Ir Int
pushCtx x = updModThen $ pushCtx' x

pushInstr :: Instr -> Ir Int
pushInstr x = updModThen $ pushInstr' x

pushVar :: Var -> Ir Int
pushVar x = updModThen $ pushVar' x

pushFun :: Fun -> Ir Int
pushFun x = updModThen $ pushFun' x

getCtx :: Ir Ctx
getCtx = state $ \m -> (ctx m, m)

getI :: Ir Int
getI = state $ \m -> (length $ instrs m, m)

-- return a formatted error when a leaf doesn't match an expected type t
typeMismatch' :: Type -> (Leaf, Type) -> Text
typeMismatch' t (Leaf p x, x') = cmpErr' p $ printf tmp fX fX' fT'
                               where
                                   fX = fmtS x
                                   fX' = fmtType x'
                                   fT' = fmtType t
                                   tmp = "type mismatch:\n \
    %s of type %s does not match expected type %s"

-- compile a function-level leaf, expecting a type t
cmpLeafAs :: Int -> Type -> Leaf -> Ir (Int, Loc)
cmpLeafAs n t x@(Leaf _ (I i)) = do{ c <- getCtx
                                   ; x' <- un $ typeof c x
                                   ; t' <- maybeOr (x' `is` t) $ exTy x'
                                   ; let tmp = Var $ n + 1
                                   ; i <- pushInstr $ Lit t' tmp $ IrInt t' i
                                   ; return (i, tmp)
                                   }
                                   where
                                       exTy x' = typeMismatch' t (x, x')

-- cmp let x: t = y on the top level
cmpTopLet :: P -> Leaf -> Type -> Leaf -> Ir Fun
cmpTopLet p x t y = do{ c <- getCtx
                      ; y' <- un $ typeof c y
                      ; t' <- maybeOr (y' `is` t) $ typeMismatch' t (y, y')
                      ; pushCtx (x, t')
                      ; (i, y) <- cmpLeafAs 0 t' y
                      ; e <- pushInstr $ Ret y
                      ; return (i, e)
                      }

-- compile top-level expressions
cmpTopLeaf :: Leaf -> Ir Fun
cmpTopLeaf (Leaf p (V "let" [ x@(Leaf _ (X _))
                            , Leaf _ (T t)
                            , y
                            ])) = cmpTopLet p x t y
cmpTopLeaf x@(Leaf p _) = un $ c s
                        where
                           c = cmpErr p
                           s = printf "cannot cmp expr %s" $ fmt x

showResult :: Show a => Res a -> String
showResult (Left e) = T.unpack e
showResult (Right x) = show x

cmpText :: Text -> IO ()
cmpText x = putStrLn $ un $ parse expr "" x
          where
              un (Left e) = errorBundlePretty e
              un (Right x) = showResult $ runStateT (cmpTopLeaf x) mkMod
