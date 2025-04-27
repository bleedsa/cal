module Verbs where

import Data.Text (Text)

import Ty

unsafeUn :: Either e s -> s
unsafeUn (Right x) = x

run :: IrFunT a -> Function
run f = unsafeUn $ runBuilder f $ mkFun "" $ mkMod ""

math :: (Type -> Loc -> Loc -> Loc -> Instr) -> Function
math f = run $ do{ x <- newVar
                 ; y <- newVar
                 ; r <- newVar
                 ; pushFInstr $ LoadLocal GenInt x "x"
                 ; pushFInstr $ LoadLocal GenInt y "y"
                 ; pushFInstr $ f GenInt r x y
                 }

add :: Function
add = math Add

neg :: Function
neg = run $ do{ x <- newVar
              ; r <- newVar
              ; pushFInstr $ LoadLocal GenInt x "x"
              ; pushFInstr $ Neg GenInt r x
              }

type Verb = (Text, Type, Function)

verbs :: [Verb]
verbs = [ ("+", typesToArrow [GenInt, GenInt, GenInt], add)
        , ("-", typesToArrow [GenInt, GenInt], neg)
        ]
