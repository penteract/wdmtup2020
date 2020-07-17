module AST where

import Data

data Statement = Equivalence Expr Expr

type Opr = Integer
type Val = Integer

data Expr = Ap Expr Expr | List [Expr] | Operator Opr | Constant Val | Fn Predef deriving (Show)

data Predef = Inc | Dec | Add | Mul | Div | Eq | Lt | Mod | Dem | Send | Neg | S | C | B | T | F | Pwr2 | I | Cone | Car | Cdr | Nil | Isnil | Vec | Draw | Checkerboard | Multipledraw | If0 | Interact | Modem | F38 | Interact | Statelessdraw | Statefuldraw deriving (Show)

predef :: Predef -> Value
predef Inc = intUnary (+1)
predef Dec = intUnary (+(-1))
predef Add = intBinary (+)
predef Mul = intBinary (*)
predef Div = intBinary quot
predef Eq = VFun (\(VInt x) -> VFun (\(VInt y) -> boolValue (x == y))  -- It's possible this will be used on things other than ints, but comparing lambda terms is uncomputable, and it would be awkward in practice.
predef Lt = VFun (\(VInt x) -> VFun (\(VInt y) -> boolValue (x < y))
predef Mod = id -- It's not clear what the semantics of the squiggle literals is meant to be.
predef Dem = id
predef Send = undefined
predef Neg = intUnary negate
predef S = VFun (\x -> VFun (\y -> VFun (\z -> apply (apply x z) (apply y z))))
predef C = VFun (\x -> VFun (\y -> VFun (\z -> apply (apply x y) z)))
predef B = VFun (\x -> VFun (\y -> VFun (\z -> apply x (apply y z))))
predef T = VFun (\x -> VFun (\y -> x))
predef F = VFun (\x -> VFun (\y -> y))
predef Pwr2 = intUnary (2^)
predef I = VFun id
predef Cone = undefined
predef Car = undefined
predef Cdr = undefined
predef Nil = undefined
predef Isnil = undefined
predef Vec = undefined
predef Draw = undefined
predef Checkerboard = undefined
predef Multipledraw = undefined
predef If0 = undefined
predef Interact = undefined
predef Modem = undefined
predef F38 = undefined
predef Interact = undefined
predef Statelessdraw = undefined
predef Statefuldraw = undefined

intUnary :: (Integer -> Integer) -> Value -> Value
intUnary f = VFun (\(VInt n) -> VInt (f n))

intBinary :: (Integer -> Integer -> Integer) -> Value -> Value
intBinary f = VFum (\(VInt n) -> intUnary (f n))

boolValue :: Bool -> Value
boolValue True = predef T
boolValue False = predef F
