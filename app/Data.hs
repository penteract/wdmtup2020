{-# LANGUAGE TemplateHaskell #-}

module Data where

import Control.Lens

data Value = VFunction (Value -> Value) | VInt Integer | VCons Value Value | VNil | VPicture --I'll deal with actual pciture data later.

-- Turns a properly terminated value list into a haskell list of values
toList :: Value -> [Value]
toList VNil = []
toList (VCons h t) = h : toList t
toList x = error ("not a list: "<>show x)

makePrisms ''Value

instance Eq Value where
  (==) (VInt n) (VInt n2) = n == n2
  (==) (VCons v v') (VCons w w') = v == w && v' == w'
  (==) VNil VNil = True
  (==) VNil _ = False
  (==) _ VNil = False
  (==) _a _b = error $ "Bad eq comparison value " <> show _a <> " " <> show _b

instance Show Value where
  show (VFunction x) = "Fun"
  show (VInt n) = "V" ++ show n
  show l@(VCons _ _) = "["++showList l
    where showList (VCons x VNil) = show x ++ "]"
          showList (VCons x xs@(VCons _ _)) = show x ++ ", " ++ showList xs
          showList (VCons x y) = show x ++ " . " ++ show y ++ "]"
  show VNil = "[]"
  show VPicture = "picture"

serialize :: Value -> String
serialize (VCons x xs) = ['1', '1'] ++ serialize x ++ serialize xs
serialize (VNil) = ['0', '0']
serialize (VInt n) = (if n >= 0 then "01" else "10") ++ replicate nybbleLength '1' ++ "0" ++ replicate (nybbleLength * 4 - length bits) '0' ++ bits
  where
    bits = convertToBits (abs n)
    nybbleLength = div (length bits + 3) 4
serialize _ = error "Serialization only available for integers, lists and nil"

convertToBits :: Integer -> String
convertToBits n = reverse $ convertToBits' n
  where
    convertToBits' 0 = []
    convertToBits' m = (if mod m 2 /= 0 then '1' else '0') : convertToBits' (div m 2)

deserialize :: String -> Value
deserialize = snd . deserialize'
  where
    deserialize' :: String -> (String, Value)
    deserialize' ('0' : '0' : xs) = (xs, VNil)
    deserialize' ('1' : '1' : xs) =
      let (xs', v0) = deserialize' xs
          (xs'', v1) = deserialize' xs'
       in (xs'', VCons v0 v1)
    deserialize' ('0' : '1' : xs) =
      let (lengthString, '0' : xs') = break (== '0') xs
          (bits, xs'') = splitAt (length lengthString * 4) xs'
       in (xs'', VInt (convertFromBits bits))
    deserialize' ('1' : '0' : xs) =
      let (lengthString, '0' : xs') = break (== '0') xs
          (bits, xs'') = splitAt (length lengthString * 4) xs'
       in (xs'', VInt (- convertFromBits bits))

convertFromBits :: String -> Integer
convertFromBits = foldl (\n b -> n * 2 + if b == '1' then 1 else 0) 0
