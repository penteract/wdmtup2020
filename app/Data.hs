module Data (
) where

data Value = VFunction (Value -> Value) | VInt Integer | VCons Value Value | VNil | VPicture --I'll deal with actual pciture data later.

instance Show Value where
    show (VFunction x) = "Fun"
    show (VInt n) = show n
    show (VCons x y) = "("++show x++", "++show y++")"
    show VNil = "n"
    show VPicture = "picture"

serialize :: Value -> [Bool]
serialize (VCons x xs) = [True,True] ++ serialize x ++ serialize xs
serialize (VNil) = [False,False]
serialize (VInt n) = (if n >= 0 then [False, True] else [True, False]) ++ replicate nybbleLength True ++ [False] ++ replicate (nybbleLength * 4 - length bits) False ++ bits
    where bits = convertToBits (abs n)
          nybbleLength = div (length bits + 3) 4
serialize _ = error "Serialization only available for integers, lists and nil"

convertToBits :: Integer -> [Bool]
convertToBits n = reverse $ convertToBits' n
    where convertToBits' 0 = []
          convertToBits' m = (mod m 2 /= 0) : convertToBits' (div m 2)

deserialize :: [Bool] -> Value
deserialize = snd . deserialize'
    where deserialize' :: [Bool] -> ([Bool], Value)
          deserialize' (False:False:xs) = (xs, VNil)
          deserialize' (True:True:xs) = let {
                  (xs', v0) = deserialize' xs;
                  (xs'', v1) = deserialize' xs'
              } in (xs'', VCons v0 v1)
          deserialize' (False:True:xs) = let {
                  (lengthString, False:xs') = break not xs;
                  (bits, xs'') = splitAt (length lengthString * 4) xs'
              } in (xs'', VInt (convertFromBits bits))
          deserialize' (True:False:xs) = let {
                  (lengthString, False:xs') = break not xs;
                  (bits, xs'') = splitAt (length lengthString * 4) xs'
              } in (xs'', VInt (-convertFromBits bits))

convertFromBits :: [Bool] -> Integer
convertFromBits = foldl (\n b -> n*2 + if b then 1 else 0) 0
