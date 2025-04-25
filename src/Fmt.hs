module Fmt where

import Text.Megaparsec
import Text.Printf
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T

import Ty

fmtP :: P -> String
fmtP (P line col) = printf "%d,%d" (unPos line) (unPos col)

fmtType :: Type -> String
fmtType (GenInt) = "{int}"
fmtType (Signed n) = printf "i%d" n
fmtType (Unsigned n) = printf "u%d" n
fmtType (Float n) = printf "f%d" n
fmtType Chr = "char"

fmtArg :: Arg -> String
fmtArg (n, t) = printf "%s: %s" (fmt n) $ fmtType t

fmtArgs :: Args -> String
fmtArgs [] = ""
fmtArgs a = (L.intercalate "->" $ map fmtArg a) ++ " -> "

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
