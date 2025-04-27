module Verbs where

import Data.Text (Text)

import Ty

type Verb = (Text, Type, Function)

unsafeUn :: Either e s -> s
unsafeUn (Right x) = x

run :: IrFunT a -> Function
run f = unsafeUn $ runBuilder f $ mkFun "" $ mkMod ""

-- math ops are all pretty much the same
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

sub :: Function
sub = math Sub

mul :: Function
mul = math Mul

div :: Function
div = math Div

modu :: Function
modu = math Modu

neg :: Function
neg = run $ do{ x <- newVar
              ; r <- newVar
              ; pushFInstr $ LoadLocal GenInt x "x"
              ; pushFInstr $ Neg GenInt r x
              }

verbs :: [Verb]
verbs = [ ("+", typesToArrow [GenInt, GenInt, GenInt], add)
        , ("-", typesToArrow [GenInt, GenInt, GenInt], sub)
        , ("*", typesToArrow [GenInt, GenInt, GenInt], mul)
        , ("%", typesToArrow [GenInt, GenInt, GenInt], Verbs.div)
        , ("!", typesToArrow [GenInt, GenInt, GenInt], modu)
        , ("-", typesToArrow [GenInt, GenInt], neg)
        ]
