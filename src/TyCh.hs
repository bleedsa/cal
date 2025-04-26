module TyCh where

import Text.Printf
import Text.Megaparsec
import Debug.Trace
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L

import Com
import Ty
import Parse
import Fmt

-- compiler error
typeErr :: P -> String -> Res Type
typeErr p s = Left $ T.pack $ printf "err: 'type: %s:\n%s" (fmtP p) s

-- is x matching type y
is :: Type -> Type -> Maybe Type
-- GenInt matches signed and unsigned ints
is GenInt y@(Signed _) = Just y
is x@(Signed _) GenInt = Just x
is GenInt y@(Unsigned _) = Just y
is x@(Unsigned _) GenInt = Just x
-- everything else just matches for equality
is x y = if x == y
         then Just x
         else Nothing

-- add args to the type context
argsToCtx :: Ctx -> Args -> Res Ctx
argsToCtx c [] = pure c
argsToCtx c ((Leaf _ (X h), ty):t) = do{ x <- argsToCtx ((h, ty):c) t
                                       ; Right x
                                       }

-- return type -> arg types -> new arrow type
sigToArrow :: Type -> [Type] -> Type
sigToArrow ret [] = Arrow Void ret
sigToArrow ret at = typesToArrow $ ret:at

-- find the leaf type in a context
findLeafTy :: Ctx -> Leaf -> Res Type
findLeafTy c (Leaf p (X x)) = map $ L.find (\(i, _) -> i == x) c 
                            where
                                err = printf "type of leaf %s not found" x
                                map (Just (_, t)) = Right t
                                map Nothing = typeErr p err

-- take two leaves, check if their types match, then return that type or error
typesMatch :: P -> Ctx -> Leaf -> Leaf -> Res Type
typesMatch p c x y = do{ x' <- typeof c x
                       ; y' <- typeof c y
                       ; case x' `is` y' of
                             Just t -> Right t
                             Nothing -> err x' y'
                       }
                       where
                           fX = fmt x
                           fY = fmt y
                           tmp = "%s (%s) does not match type of %s (%s) in add"
                           err x y = let tX = fmtType x
                                         tY = fmtType y
                                     in typeErr p $ printf tmp fX tX fY tY

typeApply1 :: P -> Ctx -> Leaf -> Leaf -> Res Type
typeApply1 p c x y = do{ x' <- typeof c x
                       ; y' <- typeof c x
                       ; dbgTrace $ fmtType x'
                       ; case x' of
                             Arrow from to -> do{ _ <- check c from y
                                                ; Right to
                                                }
                             ty -> typeErr p $ printf err fX (fmtType ty)
                       }
                       where
                           fX = fmt x
                           err = "%s is used as a function but it is %s"
                          

typeofV :: P -> Ctx -> Text -> [Leaf] -> Res Type
typeofV p c "+" [x, y] = typesMatch p c x y
typeofV p c "!" [x] = do{ x' <- typeof c x
                        ; case x' `is` GenInt of
                              Just t -> Right $ Slice t
                              Nothing -> typeErr p "iota on non-integer"
                        }
typeofV p c "@" [x, y] = typeApply1 p c x y
typeofV p _ v a = typeErr p $ printf "cannot type verb %s" $ fmtS $ V v a

typeofS :: P -> Ctx -> S -> Res Type
typeofS _ _ (I _) = Right GenInt
typeofS p c (V v a) = typeofV p c v a
typeofS p _ x = typeErr p $ printf "cannot type S expr %s" $ fmtS x

-- pos -> ctx -> fun -> return type -> args types -> body expressions
typeofFun :: P -> Ctx -> Leaf -> Type -> Args -> [Leaf] -> Res Type
typeofFun p c x r a e = do{ c' <- argsToCtx c a
                          ; t <- typeof c' $ last e
                          ; case t `is` r of
                                Just t -> Right $ sigToArrow t at
                                Nothing -> typeErr p $ printf err ft fe
                          }
                          where
                              at = map argType a
                              fe = fmt $ last e
                              ft = fmtType r
                              err = "ret type %s does not match type of expr %s"

typeof :: Ctx -> Leaf -> Res Type
typeof c x@(Leaf _ (X _)) = findLeafTy c x
typeof c x@(Leaf p (O (Sig r a) e)) = typeofFun p c x r a e
typeof c (Leaf p s) = typeofS p c s

typesof :: Ctx -> [Leaf] -> Res [Type]
typesof c [] = Right []
typesof c (h:t) = do{ h' <- typeof c h
                    ; t' <- typesof c t
                    ; Right $ h':t'
                    }

check :: Ctx -> Type -> Leaf -> Res ()
check c t x = do{ t' <- typeof c x
                ; case t `is` t' of
                      Nothing -> let e = "%s has type %s but is used as %s"
                                     fX = fmt x
                                     fT' = fmtType t'
                                     fT = fmtType t
                                 in do{ _ <- typeErr p $ printf e fX fT' fT
                                      ; pure ()
                                      }
                      Just _ -> pure ()
                }
                where
                    p = leafP x

typeTxt :: Text -> IO ()
typeTxt x = putStrLn $ un p
          where
              p = parse expr "" x
              un' (Left e) = T.unpack e
              un' (Right x) = show x
              un (Left e) = errorBundlePretty e
              un (Right x) = un' $ typeof mkCtx x
