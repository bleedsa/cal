module Ty where

import Control.Monad.State
import Data.Text (Text)
import Text.Megaparsec (Pos, unPos)
import qualified Data.List as L

import Com

type Named a = (Text, a)

type Typed a = (Type, a)

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
          | Array Type Int  -- type, size
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
       | A [Leaf]      -- array
       | T Type        -- typename
       | X Text        -- identifier
       | M Leaf [Leaf]
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

arrowRet :: Type -> Type
arrowRet (Arrow fr to) = arrowRet to
arrowRet t = t

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
           -- load a local into a location
           | LoadLocal Type Loc Text
           -- load a global into a location
           | LoadGlobalFun Type Loc Text
           -- push fn param with type T and location x
           | Param Type Loc
           -- call fn y with pushed params into x
           | Call Type Loc Loc
           -- return the value at loc
           | Ret Loc
           -- math: all locations have type t, x := y op z
           | Add Type Loc Loc Loc
           | Sub Type Loc Loc Loc
           | Div Type Loc Loc Loc
           | Mul Type Loc Loc Loc
           | Modu Type Loc Loc Loc
           -- x := -y
           | Neg Type Loc Loc
           -- x := new t[size]
           | NewArray Type Loc Int
           -- x[i] := y
           | SetArray Loc Loc Loc
           deriving (Show, Eq)

instrRets :: Instr -> Maybe Loc
instrRets (Push _ x) = pure x
instrRets (Pop _ x) = pure x
instrRets (Lit _ x _) = pure x
instrRets (Local _ _) = Nothing
instrRets (LoadLocal _ x _) = pure x
instrRets (LoadGlobalFun _ x _) = pure x
instrRets (Param _ _) = Nothing
instrRets (Call _ x _) = pure x
instrRets (Ret x) = pure x
instrRets (Add _ x _ _) = pure x
instrRets (Sub _ x _ _) = pure x
instrRets (Div _ x _ _) = pure x
instrRets (Mul _ x _ _) = pure x
instrRets (Modu _ x _ _) = pure x
instrRets (Neg _ x _) = pure x
instrRets (NewArray _ x _) = pure x
instrRets (SetArray _ x _) = Nothing

-- global var in module
type Var = (Text, IrVal)

-- module function is a body of instructions from x..y
type Body = (Int, Int)

-- a value
data IrVal = IrInt Type Int
           | IrFun Type Int
           | IrArr Type Int [IrVal]
           deriving (Show, Eq)

-- a module builder
data Mod = Mod { src :: Text
               -- type context
               , ctx :: Ctx
               -- global vars
               , vars :: [Var]
               -- global functions
               , funs :: [Named (Typed Function)]
               }
         deriving (Show, Eq)

-- a function builder
data Function = Function { fsrc :: Text
                         , finstrs :: [Instr]
                         , fctx :: Ctx
                         , flocals :: [Named Type]
                         , fargs :: [Named Type]
                         , ftmp :: Int
                         , fmod :: Mod
                         }
              deriving (Show, Eq)

data Loc = Var Int
         deriving (Show, Eq)

data Loadable = LocalVar (Named Type)
              | GlobalFun (Named Type)
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
                     , fargs = []
                     , ftmp = 0
                     , fmod = m
                     }

funRets :: Function ->  Maybe Loc
funRets f = instrRets $ last $ finstrs f

getFromMod :: (Mod -> a) -> IrModT a
getFromMod f = state $ \m -> (f m, m)

getFromFun :: (Function -> a) -> IrFunT a
getFromFun f = state $ \m -> (f m, m)

-- unwrap an Either into an Ir err
un :: Either e a -> IrMonad e s a
un (Right x) = pure x
un (Left e) = lift $ Left e

-- unwrap a maybe or return 
maybeOr :: Maybe a -> IrMonad e s e -> IrMonad e s a
maybeOr (Just x) _ = pure x
maybeOr Nothing f = do{ x <- f
                      ; lift $ Left x
                      }

-- find a local variable n in a function context
fndFLocal :: Text -> IrFunT (Maybe Loadable)
fndFLocal n = do{ l <- getFLocals
                ; a <- getFArgs
                ; return $ fmap LocalVar $ L.find ((n==) . fst) $ l ++ a
                }

-- find a super module function in a function context
fndMFun :: Text -> IrFunT (Maybe Loadable)
fndMFun n = do{ mod <- getFMod
              ; return $ fmap lod $ L.find fnd $ funs mod
              }
              where
                  fnd = (n==) . fst
                  lod (n, (t, _)) = GlobalFun (n, t)

-- find an X in a function
fndFX :: Text -> IrFunT (Maybe Loadable)
fndFX n = do{ l <- fndFLocal n
            ; case l of
                  x@(Just _) -> pure x
                  Nothing -> fndMFun n
            }

joinAndIdx :: a -> [a] -> (Int, [a])
joinAndIdx x v = (i, v ++ [x])
               where
                   i = length v

pushCtx' :: (Text, Type) -> Mod -> (Int, Mod)
pushCtx' x m = let (i, v) = joinAndIdx x $ ctx m
               in (i, Mod { ctx = x:ctx m
                          , vars = vars m
                          , funs = funs m
                          , src = src m
                          })

pushVar' :: Var -> Mod -> (Int, Mod)
pushVar' x m = let (i, v) = joinAndIdx x $ vars m
               in (i, Mod { ctx = ctx m
                          , vars = v
                          , funs = funs m
                          , src = src m
                          })

pushFun' :: Named (Typed Function) -> Mod -> (Int, Mod)
pushFun' x m = let (i, v) = joinAndIdx x $ funs m
               in (i, Mod { ctx = ctx m
                          , vars = vars m
                          , funs = v
                          , src = src m
                          })

incVar :: Function -> Function
incVar f = Function { fsrc = fsrc f
                    , finstrs = finstrs f
                    , fctx = fctx f
                    , flocals = flocals f
                    , ftmp = ftmp f + 1
                    , fmod = fmod f
                    , fargs = fargs f
                    }

pushFInstr' :: Instr -> Function -> Function
pushFInstr' x f = Function { fsrc = fsrc f
                           , finstrs = finstrs f ++ [x]
                           , fctx = fctx f
                           , flocals = flocals f
                           , ftmp = ftmp f
                           , fmod = fmod f
                           , fargs = fargs f
                           }

pushFCtx' :: (Text, Type) -> Function -> Function
pushFCtx' x f = Function { fsrc = fsrc f
                         , finstrs = finstrs f
                         , fctx = x:fctx f
                         , flocals = flocals f
                         , ftmp = ftmp f
                         , fmod = fmod f
                         , fargs = fargs f
                         }

pushFLocal' :: Named Type -> Function -> Function
pushFLocal' x f = Function { fsrc = fsrc f
                           , finstrs = finstrs f
                           , fctx = fctx f
                           , flocals = x:flocals f
                           , ftmp = ftmp f
                           , fmod = fmod f
                           , fargs = fargs f
                           }

pushFArg' :: Named Type -> Function -> Function
pushFArg' x f = Function { fsrc = fsrc f
                         , finstrs = finstrs f
                         , fctx = fctx f
                         , flocals = flocals f
                         , ftmp = ftmp f
                         , fmod = fmod f
                         , fargs = x:fargs f
                         }

joinFCtx' :: [(Text, Type)] -> Function -> Function
joinFCtx' x f = Function { fsrc = fsrc f
                         , finstrs = finstrs f
                         , fctx = x ++ fctx f
                         , flocals = flocals f
                         , ftmp = ftmp f
                         , fmod = fmod f
                         , fargs = fargs f
                         }

-- return from a state update lambda with () and the new state
retNil :: a -> ((), a)
retNil x = ((), x)

-- update a module with a function that takes a Mod and returns a new Mod
updMod :: (Mod -> Mod) -> IrModT ()
updMod f = state $ \m -> retNil $ f m

updModThen :: (Mod -> (a, Mod)) -> IrModT a
updModThen f = state $ \m -> let (i, m') = f m
                             in (i, m')
updFun :: (Function -> Function) -> IrFunT ()
updFun f = state $ \m -> retNil $ f m

updFunThen :: (Function -> (a, Function)) -> IrFunT a
updFunThen f = state $ \m -> f m

pushCtx :: (Text, Type) -> IrModT Int
pushCtx x = updModThen $ pushCtx' x

pushVar :: Var -> IrModT Int
pushVar x = updModThen $ pushVar' x

pushFun :: Named (Typed Function) -> IrModT Int
pushFun x = updModThen $ pushFun' x

pushFInstr :: Instr -> IrFunT ()
pushFInstr x = updFun $ pushFInstr' x

joinFCtx :: [(Text, Type)] -> IrFunT ()
joinFCtx x = updFun $ joinFCtx' x

pushFCtx :: (Text, Type) -> IrFunT ()
pushFCtx x = updFun $ pushFCtx' x

pushFLocal :: Named Type -> IrFunT ()
pushFLocal x = updFun $ pushFLocal' x

pushFArg :: Named Type -> IrFunT ()
pushFArg x = updFun $ pushFArg' x

getCtx :: IrModT Ctx
getCtx = getFromMod ctx

getSrc :: IrModT Text
getSrc = getFromMod src

getMod :: IrModT Mod
getMod = state $ \m -> (m, m)

getFSrc :: IrFunT Text
getFSrc = getFromFun fsrc

getFCtx :: IrFunT Ctx
getFCtx = getFromFun fctx

getFLocals :: IrFunT [Named Type]
getFLocals = getFromFun flocals

getFArgs :: IrFunT [Named Type]
getFArgs = getFromFun fargs

getFMod :: IrFunT Mod
getFMod = getFromFun fmod

newVar :: IrFunT Loc
newVar = do{ n <- getFromFun ftmp
           ; updFun incVar
           ; return $ Var n
           }

runIrBuilder :: IrMonad e s a -> s -> Either e s
runIrBuilder ir s = do{ (_, x) <- runStateT ir s
                      ; Right x
                      }

data CcInstr = Mov 
              deriving (Show, Eq)

data CcPrc = CcPrc { prcSrc :: Text
                    , prcObj :: CcObj
                    , prcInstrs :: [String]
                    }
             deriving (Show, Eq)

data CcObj = CcObj { objSrc :: Text
                    , objMod :: Mod
                    , objPrcs :: [CcPrc]
                    }
                    deriving (Show, Eq)

type CcMonad e s a = StateT s (Either e) a

type CcPrcT a = CcMonad Text CcPrc a

type CcObjT a = CcMonad Text CcObj a

mkPrc :: Text -> CcObj -> CcPrc
mkPrc src obj = CcPrc { prcSrc = src
                       , prcObj = obj
                       , prcInstrs = []
                       }

mkObj :: Text -> Mod -> CcObj
mkObj src mod = CcObj { objSrc = src
                       , objMod = mod
                       , objPrcs = []
                       }

getFromState :: (s -> a) -> StateT s (Either e) a
getFromState f = state $ \m -> (f m, m)

pushPInstr' :: CcPrc -> String -> CcPrc
pushPInstr' p i = CcPrc { prcSrc = prcSrc p
                        , prcObj = prcObj p
                        , prcInstrs = i:prcInstrs p
                        }

pushPInstr :: String -> CcPrcT ()
pushPInstr i = state $ \m -> ((), pushPInstr' m i)

pushOPrc' :: CcObj -> CcPrc -> CcObj
pushOPrc' o p = CcObj { objSrc = objSrc o
                      , objMod = objMod o
                      , objPrcs = p:objPrcs o
                      }

pushOPrc :: CcPrc -> CcObjT ()
pushOPrc p = state $ \m -> ((), pushOPrc' m p)

getOSrc :: CcObjT Text
getOSrc = getFromState objSrc

getOMod :: CcObjT Mod
getOMod = getFromState objMod

getOObj :: CcObjT CcObj
getOObj = getFromState identity

getPSrc :: CcPrcT Text
getPSrc = getFromState prcSrc

getPObj :: CcPrcT CcObj
getPObj = getFromState prcObj
