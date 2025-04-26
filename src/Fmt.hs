{-# LANGUAGE RecordWildCards #-}

module Fmt where

import Text.Megaparsec
import Text.Printf
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T

import Ty
import Com

fmtP :: P -> String
fmtP (P line col) = printf "%d,%d" (unPos line) (unPos col)

fmtType :: Type -> String
fmtType Void = "void"
fmtType GenInt = "{int}"
fmtType (Signed n) = printf "i%d" n
fmtType (Unsigned n) = printf "u%d" n
fmtType (Float n) = printf "f%d" n
fmtType Chr = "char"
fmtType (Slice t) = printf "[%s]" $ fmtType t
fmtType (Arrow x y) = printf "%s -> %s" x' y'
                    where
                        x' = fmtType x
                        y' = fmtType y

fmtArg :: Arg -> String
fmtArg (n, t) = printf "%s: %s" (fmt n) $ fmtType t

fmtArgs :: Args -> String
fmtArgs [] = ""
fmtArgs a = (L.intercalate " -> " $ map fmtArg a) ++ " -> "

fmtSig :: Sig -> String
fmtSig (Sig ret args) = printf "%s%s" (fmtArgs args) $ fmtType ret

fmtS :: S -> String
fmtS (I x) = printf "%d" x
fmtS (F x) = printf "%f" x
fmtS (X x) = T.unpack x
fmtS (A x) = printf "(%s)" $ L.intercalate ";" $ map fmt x
fmtS (O s x) = printf "{[%s] %s}" (fmtSig s) (L.intercalate ";" $ map fmt x)
fmtS (V v [x, y]) = printf "%s%s%s" (fmt x) v (fmt y)
fmtS (V v [x]) = printf "%s%s" v $ fmt x
fmtS (V v a) = printf "%s[%s]" v $ L.intercalate ";" $ map fmt a
fmtS x = show x

fmt :: Leaf -> String
fmt (Leaf _ s) = fmtS s

fmtInstr :: Instr -> String
fmtInstr i = show i

fmtFun :: (Text, Function) -> String
fmtFun (n, Function{..}) = printf "%s:\n%s" n $ L.intercalate "\n" m
                         where
                             m = map (\x -> "    " ++ fmtInstr x) finstrs

fmtMod :: Mod -> String
fmtMod Mod{..} = printf tmp fs
               where
                   tmp = "funs\n\
====\n\
%s"
                   fs = L.intercalate "\n------\n" $ map fmtFun funs

-- format pointing to part of the source
fmtPtTo :: Text -> (Int, Int) -> String
fmtPtTo src (ln, col) = printf "    %s\n    %s" (lnOfSrc src ln) (arrowTxt col)
