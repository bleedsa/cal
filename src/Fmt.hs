module Fmt where

import Text.Printf
import qualified Data.List as L
import Data.Text (Text)
import qualified Data.Text as T

import Ty

fmtP :: P -> String
fmtP (P line col) = printf "%s,%s" (show line) (show col)

fmtType :: Type -> String
fmtType (Signed n) = printf "i%d" n
fmtType (Unsigned n) = printf "u%d" n
fmtType (Float n) = printf "f%d" n
fmtType Chr = "char"

fmtArg :: Arg -> String
fmtArg (n, t) = printf "%s: %s" n $ fmtType t

fmtArgs :: Args -> String
fmtArgs [] = ""
fmtArgs a = (L.intercalate "->" $ map fmtArg a) ++ " -> "

fmtSig :: Sig -> String
fmtSig (Sig ret args) = printf "%s%s" (fmtArgs args) $ fmtType ret

fmtS :: S -> String
fmtS (I x) = printf "%d" x
fmtS (F x) = printf "%f" x
fmtS (X x) = T.unpack x
fmtS (O s x) = printf "{[%s] %s}" (fmtSig s) (L.intercalate ";" $ map fmt x)
fmtS (V v [x, y]) = printf "%s%s%s" (fmt x) v (fmt y)
fmtS (V v [x]) = printf "%s%s" v $ fmt x
fmtS (V v a) = printf "%s[%s]" v $ L.intercalate ";" $ map fmt a

fmt :: Leaf -> String
fmt (Leaf _ s) = fmtS s
