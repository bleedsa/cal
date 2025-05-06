module Verbs where

import Data.Text (Text)

import Ty

type Verb = (Text, Type, Function)

unsafeUn :: Either e s -> s
unsafeUn (Right x) = x

run :: IrFunT a -> Function
run f = unsafeUn $ runIrBuilder f $ mkFun "" $ mkMod ""

-- math ops are all pretty much the same
-- dyads created with this function
mathD :: Type -> (Type -> Loc -> Loc -> Loc -> Instr) -> [Loc] -> Function
mathD t f [r, x, y] = run $ pushFInstr $ f t r x y

-- monadic math operators
mathM :: Type -> (Type -> Loc -> Loc -> Instr) -> [Loc] -> Function
mathM t f [r, x] = run $ pushFInstr $ f t r x

add :: Type -> [Loc] -> Function
add t a  = mathD t Add a

sub :: Type -> [Loc] -> Function
sub t a = mathD t Sub a

mul :: Type -> [Loc] -> Function
mul t a = mathD t Mul a

div :: Type -> [Loc] -> Function
div t a = mathD t Div a

modu :: Type -> [Loc] -> Function
modu t a = mathD t Modu a

neg :: Type -> [Loc] -> Function
neg t a = mathM t Neg a

mathVs :: [Loc] -> Type -> [Verb]
mathVs a t = [ ("+", typesToArrow [t, t, t], add t a)
             , ("-", typesToArrow [t, t, t], sub t a)
             , ("*", typesToArrow [t, t, t], mul t a)
             , ("%", typesToArrow [t, t, t], Verbs.div t a)
             , ("!", typesToArrow [t, t, t], modu t a)
             , ("-", typesToArrow [t, t],    neg t a)
             ]

mathSigned :: [Loc] -> [Verb]
mathSigned a = concat $ map (mathVs a . Signed) [64, 32, 16, 8]

mathUnsigned :: [Loc] -> [Verb]
mathUnsigned a = concat $ map (mathVs a . Unsigned) [64, 32, 16, 8]

verbs :: [Loc] -> [Verb]
verbs a = concat [ -- mathVs GenInt
                 -- , mathSigned
                   mathUnsigned a
                 , mathSigned a
                 ]
