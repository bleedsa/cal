module Ir where

import Debug.Trace
import Data.Text (Text)
import Control.Monad.State
import Text.Printf
import Text.Megaparsec (Pos)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text as T

import Com
import Parse
import Ty
import TyCh
import Fmt

cmpErr' :: P -> String -> Text
cmpErr' p x = T.pack $ printf "err: 'compile: %s\n%s" (fmtP p) x

cmpErr :: P -> String -> Res a
cmpErr p x = Left $ cmpErr' p x

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

pushFun' :: Named Function -> Mod -> (Int, Mod)
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
                    } 

pushFInstr' :: Instr -> Function -> Function
pushFInstr' x f = Function { fsrc = fsrc f
                           , finstrs = finstrs f ++ [x]
                           , fctx = fctx f
                           , flocals = flocals f
                           , ftmp = ftmp f
                           , fmod = fmod f
                           }

pushFCtx' :: (Text, Type) -> Function -> Function
pushFCtx' x f = Function { fsrc = fsrc f
                         , finstrs = finstrs f
                         , fctx = x:fctx f
                         , flocals = flocals f
                         , ftmp = ftmp f
                         , fmod = fmod f
                         }

joinFCtx' :: [(Text, Type)] -> Function -> Function
joinFCtx' x f = Function { fsrc = fsrc f
                         , finstrs = finstrs f
                         , fctx = x ++ fctx f
                         , flocals = flocals f
                         , ftmp = ftmp f
                         , fmod = fmod f
                         }

{--
 - wrap some common ops with State
 --}

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

pushFun :: Named Function -> IrModT Int
pushFun x = updModThen $ pushFun' x

pushFInstr :: Instr -> IrFunT ()
pushFInstr x = updFun $ pushFInstr' x

joinFCtx :: [(Text, Type)] -> IrFunT ()
joinFCtx x = updFun $ joinFCtx' x

pushFCtx :: (Text, Type) -> IrFunT ()
pushFCtx x = updFun $ pushFCtx' x

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

newVar :: IrFunT Loc
newVar = do{ n <- getFromFun ftmp
           ; updFun incVar
           ; return $ Var n
           }

typeNeq' :: Text -> Type -> (Leaf, Type) -> Text
typeNeq' src t (Leaf p x, x') = c $ printf tmp fX fX' fT' $ pt src
                             where
                                 c = cmpErr' p
                                 fX = fmtS x
                                 fX' = fmtType x'
                                 fT' = fmtType t
                                 pt src = fmtPtTo src $ pExt p
                                 tmp = "type mismatch:\n\
%s of type %s does not match expected type %s\n\
%s"

-- return a formatted error when a leaf doesn't match an expected type t
typeNeq :: Type -> (Leaf, Type) -> IrFunT Text
typeNeq t x = do{ src <- getFSrc
                ; return $ typeNeq' src t x
                }

expTy :: Type -> Leaf -> IrFunT Type
expTy t x = do{ c <- getFCtx
              ; x' <- un $ typeof c x
              ; maybeOr (x' `is` t) $ typeNeq t (x, x')
              }

cmpArgs :: Args -> IrFunT ()
cmpArgs [] = pure ()
cmpArgs ((Leaf _ (X x), ty):t) = do{ _ <- pushFInstr $ Local ty x
                                   ; pushFCtx (x, ty)
                                   ; cmpArgs t
                                   }

cmpExprs :: [Leaf] -> IrFunT Loc
cmpExprs [x] = cmpLeaf x
cmpExprs (h:t) = do{ cmpLeaf h
                   ; cmpExprs t
                   }

-- pos -> expected type -> fun -> return type -> arg types -> exprs
cmpLam :: P -> Type -> Leaf -> Type -> Args -> [Leaf] -> IrFunT Loc
cmpLam p t x r a e = do{ c <- getFCtx
                       ; cmpArgs a
                       ; r' <- expTy r $ last e
                       ; cmpExprs e
                       }

cmpLeafAs :: Type -> Leaf -> IrFunT Loc
cmpLeafAs t x@(Leaf p (I i)) = do{ t' <- expTy t x
                                 ; tmp <- newVar
                                 ; pushFInstr $ Lit t' tmp $ IrInt t' i
                                 ; return tmp
                                 }
cmpLeafAs t x@(Leaf p (O (Sig r a) e)) = cmpLam p t x r a e
cmpLeafAs t (Leaf p s) = do{ src <- getFSrc
                           ; let e = printf "cannot compile leaf %s as type %s\n%s"
                                            (fmtS s) (fmtType t) (errPtr src)
                           ; un $ cmpErr p e
                           }
                           where
                               errPtr src = fmtPtTo src $ pExt p

cmpLeaf :: Leaf -> IrFunT Loc
cmpLeaf x = do{ c <- getFCtx
              ; t <- un $ typeof c x
              ; cmpLeafAs t x
              }

-- cmp let x: t = y on the top level
cmpTopLet3 :: P -> Text -> Type -> Leaf -> IrModT Function
cmpTopLet3 p x t y = do{ c <- getCtx
                       ; src <- getSrc
                       ; y' <- un $ typeof c y
                       ; t' <- maybeOr (y' `is` t) $ pure $ typeNeq' src t (y, y')
                       ; pushCtx (x, t')
                       ; mod <- getMod
                       ; let fun = do{ ret <- cmpLeafAs t' y
                                     ; pushFInstr $ Ret ret
                                     }
                       ; y <- un $ runBuilder fun $ mkFun src mod
                       ; pushFun (x, y)
                       ; return y
                       }

cmpTopLet2 :: P -> Text -> Leaf -> IrModT Function
cmpTopLet2 p x y = do{ c <- getCtx
                     ; y' <- un $ typeof c y
                     ; cmpTopLet3 p x y' y
                     }

-- compile top-level expressions
cmpTopLeaf :: Leaf -> IrModT Function
cmpTopLeaf (Leaf p (V "let" [ Leaf _ (X x)
                            , Leaf _ (T t)
                            , y
                            ])) = cmpTopLet3 p x t y
cmpTopLeaf (Leaf p (V "let" [Leaf _ (X x), y])) = cmpTopLet2 p x y
cmpTopLeaf x@(Leaf p _) = un $ c s
                        where
                           c = cmpErr p
                           s = printf "cannot compile top level expr %s" $ fmt x

cmpTopLeaves :: [Leaf] -> IrModT ()
cmpTopLeaves [] = pure ()
cmpTopLeaves (h:t) = do{ cmpTopLeaf h
                       ; cmpTopLeaves t
                       }

showResult :: Show a => Res a -> String
showResult (Left e) = T.unpack e
showResult (Right x) = show x

fmtResult :: Res (a, Mod) -> String
fmtResult (Left e) = T.unpack e
fmtResult (Right (_, x)) = fmtMod x

runBuilder :: IrMonad e s a -> s -> Either e s
runBuilder ir s = do{ (_, x) <- runStateT ir s
                    ; Right x
                    }

cmpTxt :: Text -> IO ()
cmpTxt x = putStrLn $ un fmtResult e
         where
             e = parse exprs "" x
             un f (Left e) = errorBundlePretty e
             un f (Right e) = f $ runStateT (cmpTopLeaves e) $ mkMod x
