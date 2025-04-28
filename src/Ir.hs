module Ir where

import Debug.Trace
import Data.Text (Text)
import Control.Monad.State
import Text.Printf
import Text.Megaparsec (Pos)
import Text.Megaparsec (parse, errorBundlePretty)
import qualified Data.Text as T
import qualified Data.List as L

import Com
import Parse
import Ty
import TyCh
import Fmt
import Verbs

cmpErr' :: P -> String -> Text
cmpErr' p x = T.pack $ printf "err: 'compile: %s\n%s" (fmtP p) x

cmpErr :: P -> String -> Res a
cmpErr p x = Left $ cmpErr' p x

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

-- return a formatted error when a verb isn't found
vNotFnd :: P -> Text -> [Leaf] -> IrFunT Text
vNotFnd p v a = do{ c <- getFCtx
                  ; src <- getFSrc
                  ; a' <- un $ typesof c a
                  ; let fA' = L.intercalate ";" $ map fmtType a'
                    in return $ e $ printf "verb overload not found: %s[%s]\n%s"
                                           v fA' $ fmtPtTo src $ pExt p
                  }
                  where
                      e = cmpErr' p

expTy :: Type -> Leaf -> IrFunT Type
expTy t x = do{ c <- getFCtx
              ; x' <- un $ typeof c x
              ; maybeOr (x' `is` t) $ typeNeq t (x, x')
              }

-- load an X (ident) into a Loc
loadX :: P -> Type -> Text -> IrFunT Loc
loadX p t n = do{ lod <- fndFX n
                ; tmp <- newVar
                ; f <- un $ lodF tmp lod
                ; pushFInstr f
                ; return tmp
                }
                where
                    -- get the type of loading function
                    -- based on the result of fndFX
                    lodF x (Just (LocalVar (n, t))) = Right $ LoadLocal t x n
                    lodF x (Just (GlobalVar (n, t))) = Right $ LoadGlobal t x n
                    lodF x Nothing = cmpErr p $ printf "cannot find variable %s to load" n

cmpArgs :: Args -> IrFunT ()
cmpArgs [] = pure ()
cmpArgs ((Leaf _ (X x), ty):t) = do{ _ <- pushFInstr $ Local ty x
                                   ; pushFCtx (x, ty)
                                   ; cmpArgs t
                                   }

cmpExprs :: [Leaf] -> IrFunT [Loc]
cmpExprs [x] = do{ x' <- cmpLeaf x
                 ; return [x']
                 }
cmpExprs (h:t) = do{ h' <- cmpLeaf h
                   ; t' <- cmpExprs t
                   ; return $ h':t'
                   }

cmpMath :: (Type -> Loc -> Loc -> Loc -> Instr) -> Type -> Leaf -> Leaf -> IrFunT Loc
cmpMath f t x y = do{ c <- getFCtx
                    ; t' <- expTy t x
                    ; t' <- expTy t' y
                    ; x' <- cmpLeafAs t' x
                    ; y' <- cmpLeafAs t' y
                    ; tmp <- newVar
                    ; pushFInstr $ f t' tmp x' y'
                    ; return tmp
                    }

-- pos -> expected type -> fun -> return type -> arg types -> exprs
cmpLam :: P -> Type -> Leaf -> Type -> Args -> [Leaf] -> IrFunT Loc
cmpLam p t x r a e = do{ c <- getFCtx
                       ; cmpArgs a
                       ; r' <- expTy r $ last e
                       ; e' <- cmpExprs e
                       ; return $ last e'
                       }

-- compile pushing parameters at locations with types
cmpParams :: [Typed Loc] -> IrFunT ()
cmpParams [] = pure ()
cmpParams ((ty, h):t) = do{ pushFInstr $ Param ty h
                          ; cmpParams t
                          }

-- compile setting the indexes of location a to the values of locations X
cmpSetIndices :: P ->  Int -> Loc -> [Loc] -> IrFunT ()
cmpSetIndices p n a [] = pure ()
cmpSetIndices p n a (h:t) = do{ c <- getFCtx
                              ; i <- cmpLeaf $ Leaf p $ I n
                              ; pushFInstr $ SetArray a i h
                              ; cmpSetIndices p (n+1) a t
                              }

-- pos -> expected type -> verb func type -> verb func -> args
cmpVerb :: P -> Text -> Type -> Type -> Function -> [Leaf] -> IrFunT Loc
cmpVerb p v e t f a = do{ c <- getFCtx
                        ; locs <- cmpExprs a
                        ; a' <- un $ typesof c a
                        ; v' <- maybeOr (getVerb v a') $ vNotFnd p v a
                        ; let prms = zip a' locs
                        ; cmpParams prms
                        ; ret <- newVar
                        ; pushFInstr $ Verb e ret v'
                        ; return ret
                        }

cmpV :: P -> Type -> Text -> [Leaf] -> IrFunT Loc
cmpV p t v a = case L.find (\(n, _, _) -> n == v) verbs of
                   Nothing -> do{ src <- getFSrc
                                ; let e = printf "verb %s not found\n%s"
                                          (fmtS $ V v a) (fmtPtTo src $ pExt p)
                                ; un $ cmpErr p e
                                }
                   Just (v, vT, vF) -> cmpVerb p v t vT vF a

cmpM :: P -> Type -> Leaf -> [Leaf] -> IrFunT Loc
cmpM p t x a = do{ c <- getFCtx
                 ; x' <- un $ typeof c x
                 ; a' <- un $ typesof c a
                 ; f <- cmpLeaf x
                 ; locs <- cmpExprs a
                 ; ret <- newVar
                 ; cmpParams $ zip a' locs
                 ; case canApply x' a' of
                       Just t' -> case t `is` t' of
                                      Just t -> pushFInstr $ Call t ret f
                                      Nothing -> let e = printf "%s doesn't match %s"
                                                                (fmtType t) (fmtType t')
                                                 in un $ cmpErr p e
                 ; return ret
                 }

cmpLeafAs :: Type -> Leaf -> IrFunT Loc
cmpLeafAs t x@(Leaf p (I i)) = do{ t' <- expTy t x
                                 ; tmp <- newVar
                                 ; pushFInstr $ Lit t' tmp $ IrInt t' i
                                 ; return tmp
                                 }
cmpLeafAs t x@(Leaf p (X i)) = do{ t' <- expTy t x
                                 ; tmp <- loadX p t' i
                                 ; return tmp
                                 }
cmpLeafAs t x@(Leaf p (A v)) = do{ t' <- expTy t x
                                 ; ret <- newVar
                                 ; pushFInstr $ NewArray t' ret sz
                                 ; locs <- cmpExprs v
                                 ; cmpSetIndices p 0 ret locs
                                 ; return ret
                                 }
                                 where
                                     sz = length v
cmpLeafAs t x@(Leaf p (V v a)) = cmpV p t v a
cmpLeafAs t (Leaf p (M x a)) = cmpM p t x a
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
                       ; pushFun (x, (t', y))
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
cmpTopLeaf x@(Leaf p _) = un $ c $ printf "cannot compile top level expr %s" $ fmt x
                        where
                           c = cmpErr p

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

cmpTxt :: Text -> IO ()
cmpTxt x = putStrLn $ un fmtResult e
         where
             e = parse top "" x
             un f (Left e) = errorBundlePretty e
             un f (Right e) = f $ runStateT (cmpTopLeaves e) $ mkMod x

cmpFile :: String -> IO ()
cmpFile x = do{ f <- readFile x
              ; cmpTxt $ T.pack f
              }
