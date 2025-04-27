module Verbs where

import Data.Text (Text)

import Ty

type Verb = (Text, Type, Function)

unsafeUn :: Either e s -> s
unsafeUn (Right x) = x

run :: IrFunT a -> Function
run f = unsafeUn $ runBuilder f $ mkFun "" $ mkMod ""

-- math ops are all pretty much the same
math :: Type -> (Type -> Loc -> Loc -> Loc -> Instr) -> Function
math t f = run $ do{ x <- newVar
                   ; y <- newVar
                   ; r <- newVar
                   ; pushFInstr $ LoadLocal t x "x"
                   ; pushFInstr $ LoadLocal t y "y"
                   ; pushFInstr $ f t r x y
                   }

add :: Type -> Function
add t = math t Add

sub :: Type -> Function
sub t = math t Sub

mul :: Type -> Function
mul t = math t Mul

div :: Type -> Function
div t = math t Div

modu :: Type -> Function
modu t = math t Modu

neg :: Function
neg = run $ do{ x <- newVar
              ; r <- newVar
              ; pushFInstr $ LoadLocal GenInt x "x"
              ; pushFInstr $ Neg GenInt r x
              }

mathVs :: Type -> [Verb]
mathVs t = [ ("+", typesToArrow [t, t, t], add t)
           , ("-", typesToArrow [t, t, t], sub t)
           , ("*", typesToArrow [t, t, t], mul t)
           , ("%", typesToArrow [t, t, t], Verbs.div t)
           , ("!", typesToArrow [t, t, t], modu t)
           ]

mathSigned :: [Verb]
mathSigned = concat $ map (mathVs . Signed) [8, 16, 32, 64]

verbs :: [Verb]
verbs = concat [ mathSigned
               , mathVs GenInt
               ]
