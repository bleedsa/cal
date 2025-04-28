module Verbs where

import Data.Text (Text)

import Ty

type Verb = (Text, Type, Function)

unsafeUn :: Either e s -> s
unsafeUn (Right x) = x

run :: IrFunT a -> Function
run f = unsafeUn $ runBuilder f $ mkFun "" $ mkMod ""

-- math ops are all pretty much the same
-- dyads created with this function
mathD :: Type -> (Type -> Loc -> Loc -> Loc -> Instr) -> Function
mathD t f = run $ do{ x <- newVar
                    ; y <- newVar
                    ; r <- newVar
                    ; pushFInstr $ LoadLocal t x "x"
                    ; pushFInstr $ LoadLocal t y "y"
                    ; pushFInstr $ f t r x y
                    }

-- monadic math operators
mathM :: Type -> (Type -> Loc -> Loc -> Instr) -> Function
mathM t f = run $ do{ x <- newVar
                    ; r <- newVar
                    ; pushFInstr $ LoadLocal t x "x"
                    ; pushFInstr $ f t r x
                    }

add :: Type -> Function
add t = mathD t Add

sub :: Type -> Function
sub t = mathD t Sub

mul :: Type -> Function
mul t = mathD t Mul

div :: Type -> Function
div t = mathD t Div

modu :: Type -> Function
modu t = mathD t Modu

neg :: Type -> Function
neg t = mathM t Neg

mathVs :: Type -> [Verb]
mathVs t = [ ("+", typesToArrow [t, t, t], add t)
           , ("-", typesToArrow [t, t, t], sub t)
           , ("*", typesToArrow [t, t, t], mul t)
           , ("%", typesToArrow [t, t, t], Verbs.div t)
           , ("!", typesToArrow [t, t, t], modu t)
           , ("-", typesToArrow [t, t],    neg t)
           ]

mathSigned :: [Verb]
mathSigned = concat $ map (mathVs . Signed) [8, 16, 32, 64]

verbs :: [Verb]
verbs = concat [ mathVs GenInt
               , mathSigned
               ]
