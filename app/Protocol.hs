{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Protocol where

import Control.Arrow
import Data.Bifunctor
import Data.List.Extra
import qualified Data.Map as M
import Data.Monoid

import Data
import Parser
import Solve
import Builtins
import Send

detectCross' :: Value -> Maybe (Integer, Integer)
detectCross' = (\[c] -> detectCross . map (\(VCons (VInt x) (VInt y)) -> (x,y)) $ toList c) . toList

detectCross :: [(Integer, Integer)] -> Maybe (Integer, Integer)
detectCross ps@(unzip -> (xs,ys)) = if null ps then Nothing else
  let cand_x = mode xs
      cand_y = mode ys
      all_ok = all (uncurry (||) . bimap (cand_x ==) (cand_y ==)) ps
      mode = fst . head . sortOn snd . M.toList . foldMap (flip M.singleton $ Sum 1)
   in if all_ok then Just (cand_x, cand_y) else Nothing

draw1 :: Integer -> Integer -> Integer -> Integer -> M.Map (Integer,Integer) Char -> String
draw1 xmin xmax ymin ymax ps = unlines $ map (\y ->
    map (flip (M.findWithDefault ' ') ps . (,y)) [xmin .. xmax]
  ) [ymin .. ymax]


drawh :: [[(Integer,Integer)]] -> String
drawh xs = unlines $ show (xmin, ymin) : map (draw1 xmin xmax ymin ymax) pointMaps
  where (xmin, xmax) = (minimum &&& maximum) (map fst $ concat xs)
        (ymin, ymax) = (minimum &&& maximum) (map snd $ concat xs)
        allPoints = M.fromList (map (,'.') $ concat xs)
        pointMaps = map (foldr (\(p,c) m -> M.insert p c m) allPoints . map (,'#')) xs


-- Takes [[(Int,Int)]] as values and returns a sequence of images
draw :: Value -> String
draw v =
  drawh (map (\ l -> map (\(VCons (VInt x) (VInt y)) -> (x,y)) (toList l) ) (toList v))

-- draw  s = ("to be drawn: "++show s)


alienInteract :: (Value -> Value) -> Value -> Value -> IO (Value,Value)
alienInteract f state vec = do
  let (flag:newState:dat:end) = toList (apply (f state) vec)
  if end/=[] then print end else return ()
  if flag==(VInt 0) then do
    putStrLn "Terminate Interaction"
    print newState
    print (serialize newState)
    putStrLn (draw dat)
    return (newState,dat)
  else do
    putStrLn "Continue Interaction"
    print newState
    resp <- send dat
    alienInteract f newState resp

-- Result:
-- Terminate Interaction
-- [V0, [V0], V0, []]
-- to be drawn: [[[V-1 . V-3], [V0 . V-3], [V1 . V-3], [V2 . V-2], [V-2 . V-1], [V-1 . V-1], [V0 . V-1], [V3 . V-1], [V-3 . V0], [V-1 . V0], [V1 . V0], [V3 . V0], [V-3 . V1], [V0 . V1], [V1 . V1], [V2 . V1], [V-2 . V2], [V-1 . V3], [V0 . V3], [V1 . V3]], [[V-7 . V-3], [V-8 . V-2]], []]

-- #39
loop39 f s v = do
  --loopn f s v 9
  (s',dat) <- alienInteract f s v
  if s==s' then return (s,dat)
  else loop39 f s' v

loopn f s v 0 = do
  print "Done"
  return s
loopn f s v n = do
  (s',_) <- alienInteract f s v
  loopn f s' v (n-1)
