module AST where

import Data

data Statement = Equivalence Expr Expr deriving Show

type Opr = Integer
type Val = Integer

data Expr = Ap Expr Expr | List [Expr] | Operator Opr | Constant Val | Fn Predef deriving (Show)

data Predef = Inc | Dec | Add | Mul | Div | Eq | Lt | Mod | Dem | Send | Neg | S | C | B | T | F | Pwr2 | I | Cons | Car | Cdr | Nil | Isnil | Vec | Draw | Checkerboard | Multipledraw | If0 | Interact | Modem | F38 | Statelessdraw | Statefuldraw deriving (Show)

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
predef Cons = VFun (\x -> VFun (\y -> VCons x y))
predef Car = VFun (\(VCons x y) -> x)
predef Cdr = VFun (\(VCons x y) -> y)
predef Nil = VNil
predef Isnil = VFun (\x -> case x of {VNil -> boolValue True; _ -> boolValue False})
predef Vec = predef Cons
predef Draw = VFun (\x -> VPicture)
predef Checkerboard = VFun (\x -> VFun (\y -> VPicture))
predef Multipledraw = VFun multipleDraw
    where multipleDraw VNil = VNil
          multipleDraw (VCons x xs) = VCons VPicture (multipleDraw xs)
predef If0 = VFun (\(VInt n) -> boolValue (n == 0))
predef Interact = undefined
predef Modem = id
predef F38 = undefined
predef Statelessdraw = VFun (\x -> VFun (\y -> vList [VInt 0, VNil, vList [ vList [y]]]))
predef Statefuldraw = VFun (\x -> VFun (\y -> vList [VInt 0, VCons y x, vList [VCons y s]]))

intUnary :: (Integer -> Integer) -> Value -> Value
intUnary f = VFun (\(VInt n) -> VInt (f n))

intBinary :: (Integer -> Integer -> Integer) -> Value -> Value
intBinary f = VFum (\(VInt n) -> intUnary (f n))

boolValue :: Bool -> Value
boolValue True = predef T
boolValue False = predef F

vList :: [Value] -> Value
vList = foldr VCons VNil
