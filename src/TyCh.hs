module TyCh where

import Text.Printf
import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L

import Com
import Ty
import Parse
import Fmt

type Ctx = [(Leaf, Type)]

mkCtx :: Ctx
mkCtx = []

-- compiler error
typeErr :: P -> String -> Either Text Type
typeErr p s = Left $ T.pack $ printf "err: 'type: %s: %s" (fmtP p) s

-- find the leaf type in a context
findLeafTy :: Ctx -> Leaf -> Either Text Type
findLeafTy c x@(Leaf p (X _)) = map opt
                              where
                                  x' = fmt x
                                  err = printf "type of leaf %s not found" x'
                                  opt = L.find (\(i, _) -> i == x) c
                                  map (Just (_, t)) = Right t
                                  map Nothing = typeErr p err

-- take two leaves, check if their types match, then return that type or error
typesMatch :: P -> Ctx -> Leaf -> Leaf -> Either Text Type
typesMatch p c x y = do{ x' <- typeof c x
                       ; y' <- typeof c y
                       ; if x' == y'
                         then Right x'
                         else let fX = fmt x
                                  fY = fmt y
                                  tmp = "%s does not match type of %s in add"
                              in typeErr p $ printf tmp fX fY
                       }

typeofV :: P -> Ctx -> Text -> [Leaf] -> Either Text Type
typeofV p c "+" [x, y] = typesMatch p c x y
typeofV p c "!" [x] = do{ x' <- typeof c x
                        ; case x' of
                              Signed 32 -> Right $ Slice $ Signed 32
                              t -> typeErr p "iota on non-integer"
                        }
typeofV p _ v a = typeErr p $ printf "cannot type verb %s" $ fmtS $ V v a

typeofS :: P -> Ctx -> S -> Either Text Type
typeofS _ _ (I _) = Right $ Signed 32
typeofS _ _ (F _) = Right $ Float 64
typeofS p c (V v a) = typeofV p c v a
typeofS _ _ x = Left $ T.pack $ printf "cannot type S expr %s" $ fmtS x

typeof :: Ctx -> Leaf -> Either Text Type
typeof c x@(Leaf _ (X _)) = findLeafTy c x
typeof c x@(Leaf p (O (Sig r a) e)) = do{ t <- typeof ((x, r):c') $ last e
                                        ; if t == r
                                          then Right $ typesToArrow at
                                          else Left $ T.pack err
                                        }
                                        where
                                            c' = argsToCtx c a
                                            at = map argType a
                                            ft = fmt $ last e
                                            err :: String
                                            err = printf "return type does not match final expr %s" ft
typeof c (Leaf p s) = typeofS p c s

argsToCtx :: Ctx -> Args -> Ctx
argsToCtx c [] = c
argsToCtx c ((h, ty):t) = argsToCtx c' t
                        where
                            c' :: Ctx
                            c' = (h, ty):c

check :: Ctx -> Type -> Leaf -> Either Text ()
check c t x = do{ t' <- typeof c x
                ; if t /= t'
                  then let e = printf "%s has type %s but is used as %s"
                               (fmt x) (show t') (show t)
                       in Left $ T.pack e
                  else pure ()
                }

typeTxt :: Text -> IO ()
typeTxt x = putStrLn $ un p
          where
              p = parse expr "" x
              un' (Left e) = T.unpack e
              un' (Right x) = show x
              un (Left e) = errorBundlePretty e
              un (Right x) = un' $ typeof mkCtx x
