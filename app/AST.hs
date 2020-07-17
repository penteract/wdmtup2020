{-# LANGUAGE TemplateHaskell #-}

module AST where

import Control.Lens
import Data

data Statement = Equivalence Expr Expr deriving (Show)

type Opr = Integer

type Val = Integer

data Predef
  = Inc
  | Dec
  | Add
  | Mul
  | Div
  | Eq
  | Lt
  | Mod
  | Dem
  | Send
  | Neg
  | S
  | C
  | B
  | T
  | F
  | Pwr2
  | I
  | Cons
  | Car
  | Cdr
  | Nil
  | Isnil
  | Vec
  | Draw
  | Checkerboard
  | Multipledraw
  | If0
  | Interact
  | Modem
  | F38
  | Statelessdraw
  | Statefuldraw
  deriving (Bounded, Enum, Show, Eq, Ord)

data Expr = Ap Expr Expr | List [Expr] | Operator Opr | Constant Val | Fn Predef deriving (Show)

makePrisms ''Expr

predef :: Predef -> Value
predef Inc = intUnary (+ 1)
predef Dec = intUnary (+ (-1))
predef Add = intBinary (+)
predef Mul = intBinary (*)
predef Div = intBinary quot
predef Eq = VFunction (\v -> VFunction (\v' -> boolValue (v == v')))
-- VFunction (\(VInt x) -> VFunction (\(VInt y) -> boolValue (x == y)))  -- It's possible this will be used on things other than ints, but comparing lambda terms is uncomputable, and it would be awkward in practice.
predef Lt = VFunction (\(VInt x) -> VFunction (\(VInt y) -> boolValue (x < y)))
predef Mod = VFunction id -- It's not clear what the semantics of the squiggle literals is meant to be.
predef Dem = VFunction id
predef Send = undefined
predef Neg = intUnary negate
predef S = VFunction (\x -> VFunction (\y -> VFunction (\z -> apply (apply x z) (apply y z))))
predef C = VFunction (\x -> VFunction (\y -> VFunction (\z -> apply (apply x y) z)))
predef B = VFunction (\x -> VFunction (\y -> VFunction (\z -> apply x (apply y z))))
predef T = VFunction (\x -> VFunction (\y -> x))
predef F = VFunction (\x -> VFunction (\y -> y))
predef Pwr2 = intUnary (2 ^)
predef I = VFunction id
predef Cons = VFunction (\x -> VFunction (\y -> VCons x y))
predef Car = VFunction (\(VCons x y) -> x)
predef Cdr = VFunction (\(VCons x y) -> y)
predef Nil = VNil
predef Isnil = VFunction (\x -> case x of VNil -> boolValue True; _ -> boolValue False)
predef Vec = predef Cons
predef Draw = VFunction (\x -> VPicture)
predef Checkerboard = VFunction (\x -> VFunction (\y -> VPicture))
predef Multipledraw = VFunction multipleDraw
  where
    multipleDraw VNil = VNil
    multipleDraw (VCons x xs) = VCons VPicture (multipleDraw xs)
predef If0 = VFunction (\(VInt n) -> boolValue (n == 0))
predef Interact = undefined
predef Modem = VFunction id
predef F38 = undefined
predef Statelessdraw = VFunction (\x -> VFunction (\y -> vList [VInt 0, VNil, vList [vList [y]]]))
predef Statefuldraw = VFunction (\x -> VFunction (\y -> vList [VInt 0, VCons y x, vList [VCons y x]]))

intUnary :: (Integer -> Integer) -> Value
intUnary f = VFunction (\(VInt n) -> VInt (f n))

intBinary :: (Integer -> Integer -> Integer) -> Value
intBinary f = VFunction (\(VInt n) -> intUnary (f n))

apply :: Value -> Value -> Value
apply (VFunction f) = f

boolValue False = predef F

vList :: [Value] -> Value
vList = foldr VCons VNil
