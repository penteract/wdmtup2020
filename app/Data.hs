module Data (
) where

data Value = VFunction (Value -> Value) | VInt Integer | VCons Value Value | VNil | Picture --I'll deal with actual pciture data later.

serialize :: Value -> [Boolean]
serialize (VList (x : xs)) = [True,True] ++ serialize x ++ serialize xs
serialize (VList []) = [False,False]
serialize (VInt n) = (if n >= 0 then [False, True] else [True, False]) ++ replicate bitLength True ++ [False] ++ replicate (nybbleLength * 4 - length bits) 0 ++ bits
    where bits = convertToBits (abs n)
          nybbleLength = div (length bits + 3) 4

convertToBits :: Integer -> [Boolean]
convertToBits n = reverse $ convertToBits' n
    where convertToBits' 0 = []
          convertToBits' m = (m % 2 == 0) : convertToBits' (div m 2)

deserialize :: [Boolean] -> Value
deserialize = snd $ deserialize'
    where deserialize' :: [Boolean] -> ([Boolean], Value)
          deserialize' (False:False:xs) = (xs, VNil)
          deserialize' (True:True:xs) = let {
                  (xs', v0) = deserialize' xs;
                  (xs'', v1) = deserialize' xs'
              } in (xs'', VCons v0 v1)
          deserialize' (False:True:xs) = let {
                  (lengthString, False:xs') = break id xs
                  (bits, xs'') = splitAt (length lengthString * 4) xs'
              } in (xs'', VInt (convertFromBits bits))
          deserialize' (True:False:xs) = let {
                  (lengthString, False:xs') = break id xs
                  (bits, xs'') = splitAt (length lengthString * 4) xs'
              } in (xs'', VInt (-convertFromBits bits))
