{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Protocol where

import Control.Arrow
import Data.Bifunctor
import Data.List.Extra
import qualified Data.Map as M
import Data.Monoid
import Data.Ord
import Debug.Trace

import Data
import Parser
import Solve
import Builtins
import Send

vPair :: Value -> (Integer, Integer)
vPair (VCons (VInt x) (VInt y)) = (x,y)

listify2 :: Value -> [[(Integer,Integer)]]
listify2 = map (map vPair . toList) . toList

-- suitable for output to gridselect.py
listify3 :: Value -> [String]
listify3 = concatMap (\(a,ps) -> map (\(x,y) -> show x <> " " <> show y <> " " <> show a) ps) . zip [1..] . listify2

detectCross' :: Value -> Maybe (Integer, Integer)
detectCross' = (\c -> detectCross .
   map (\(VCons (VInt x) (VInt y)) -> (x,y)) $ (c>>= toList)) . toList

detectCross :: [(Integer, Integer)] -> Maybe (Integer, Integer)
detectCross ps@(unzip -> (xs,ys)) = if null ps then Nothing else
  let cand_x = mode xs
      cand_y = mode ys
      all_ok = all (uncurry (||) . bimap (cand_x ==) (cand_y ==)) ps
   in if True then Just (cand_x, cand_y) else Nothing

mode :: [Integer] -> Integer
mode = fst . head . sortOn (Down . snd) . M.toList . M.unionsWith (<>) . map (flip M.singleton (Sum 1))

draw1 :: Integer -> Integer -> Integer -> Integer -> M.Map (Integer,Integer) String -> String
draw1 xmin xmax ymin ymax ps = unlines $ map (\y ->
    concatMap (flip (M.findWithDefault " ") ps . (,y)) [xmin .. xmax]
  ) [ymin .. ymax]

selectMiddle :: [(Integer, Integer)] -> [(Integer, Integer)]
selectMiddle ps@(unzip ->(xs,ys)) =
  let threshold = 0.05
      (xmin, xmax) = (minimum &&& maximum) xs
      xrange = xmax - xmin
      xmid = fromIntegral xrange / 2
      xthresh = threshold * fromIntegral xrange
      (xlo, xhi) = (xmid - xthresh, xmid + xthresh)
      (ymin, ymax) = (minimum &&& maximum) ys
      yrange = ymax - ymin
      ymid = fromIntegral yrange / 2
      ythresh = threshold * fromIntegral yrange
      (ylo, yhi) = (ymid - ythresh, ymid + ythresh)
      inBounds (fromIntegral -> x, fromIntegral -> y)
        = xlo <= x && x <= xhi && ylo <= y && y <= yhi
   in filter inBounds ps

drawh :: (Integer, Integer) -> [[(Integer,Integer)]] -> String
drawh cursor xs = unlines $ show (xmin, ymin) : map (draw1 xmin xmax ymin ymax . addCursor) pointMaps
  where (xmin, xmax) = (minimum &&& maximum) (map fst $ concat xs)
        (ymin, ymax) = (minimum &&& maximum) (map snd $ concat xs)
        allPoints = M.fromList (map (,".") $ concat xs)
        pointMaps = map (foldr (\(p,c) m -> M.insert p c m) allPoints . map (,"#")) xs
        addCursor = M.alter (\case
            Nothing -> Just "\x1b[101m \x1b[49m"
            Just s -> Just ("\x1b[101m"++s++"\x1b[49m")
          ) cursor


-- Takes [[(Int,Int)]] as values and returns a sequence of images
draw :: (Integer,Integer) -> Value -> String
draw cursor v = drawh cursor (map (\ l -> map (\(VCons (VInt x) (VInt y)) -> (x,y)) (toList l) ) (toList v))
--draw  s = ("to be drawn: "++show (length$toList s))


alienInteract :: (Value -> Value) -> Value -> Value -> IO (Value,Value)
alienInteract f state vec = do
  let (flag:newState:dat:end) = toList (apply (f state) vec)
  if end/=[] then print end else return ()
  if flag==(VInt 0) then do
    putStrLn "Terminate Interaction"
    case pp newState of
      Just a ->  putStrLn ("state: "++a)
      Nothing -> putStrLn "unprintable"
    print newState
    print (serialize newState)
    --putStrLn (draw (0,0) dat)
    return (newState,dat)
  else do
    putStrLn "Continue Interaction"
    case pp newState of
      Just a ->  putStrLn ("state: "++a)
      Nothing -> putStrLn "unprintable"
    print newState
    print (serialize newState)
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
