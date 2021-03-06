module Builtins where

import AST
import Data
import Send
import System.IO.Unsafe

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
predef Send = VFunction (\ x -> unsafePerformIO (send x))
predef Neg = intUnary negate
predef S = VFunction (\x -> VFunction (\y -> VFunction (\z -> apply (apply x z) (apply y z))))
predef C = VFunction (\x -> VFunction (\y -> VFunction (\z -> apply (apply x z) y)))
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
predef Interact = VFunction (\x -> VFunction (\y -> VFunction (\z -> a2 (predef F38) x (a2 x y z))))
predef Modem = VFunction id
predef F38 = VFunction (\x -> VFunction (\y -> undefined)) --   ap ap ap if0 ap car x0 ( ap modem ap car ap cdr x0 , ap multipledraw ap car ap cdr ap cdr x0 )
                      -- ap ap ap interact x2 ap modem ap car ap cdr x0 ap send ap car ap cdr ap cdr x0
predef Statelessdraw = VFunction (\x -> VFunction (\y -> vList [VInt 0, VNil, vList [vList [y]]]))
predef Statefuldraw = VFunction (\x -> VFunction (\y -> vList [VInt 0, VCons y x, vList [VCons y x]]))

a2 f x y = apply (apply f x) y



intUnary :: (Integer -> Integer) -> Value
intUnary f = VFunction (\(VInt n) -> VInt (f n))

intBinary :: (Integer -> Integer -> Integer) -> Value
intBinary f = VFunction (\(VInt n) -> intUnary (f n))

apply :: Value -> Value -> Value
apply (VFunction f) x = f x
apply (VCons a b) x = apply (apply x a) b
apply f x = error ("Bad function application: " <> show f <> "\nbeing applied to" <> show x)

boolValue True = predef T
boolValue False = predef F

vList :: [Value] -> Value
vList = foldr VCons VNil
