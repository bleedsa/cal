module TyCh where

import Text.Printf
import Text.Megaparsec
import Data.Text (Text)
import qualified Data.Text as T

import Com
import Ty
import Parse
import Fmt

type Ctx = [(Leaf, Type)]

mkCtx :: Ctx
mkCtx = []

typeofV :: Ctx -> Text -> [Leaf] -> Either Text Type
typeofV c "+" [x, y] = do{ check c (Signed 32) x
                         ; check c (Signed 32) y
                         ; Right $ Signed 32
                         }
typeofV _ v a = Left $ T.pack $ printf "cannot type verb %s" $ fmtS $ V v a

typeofS :: Ctx -> S -> Either Text Type
typeofS _ (I _) = Right $ Signed 32
typeofS _ (F _) = Right $ Float 64
typeofS c (V v a) = typeofV c v a
typeofS _ x = Left $ T.pack $ printf "cannot type S expr %s" $ fmtS x

typeof :: Ctx -> Leaf -> Either Text Type
typeof c x@(Leaf p (O (Sig r a) e)) = do{ t <- typeof ((x, r):c) $ last e
                                        ; if t == r
                                          then Right $ argsToType $ ("", t):a
                                          else Left $ T.pack err
                                        }
                                        where
                                            ft = fmt $ last e
                                            err :: String
                                            err = printf "return type does not match final expr %s" ft
typeof c (Leaf _ s) = typeofS c s

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
