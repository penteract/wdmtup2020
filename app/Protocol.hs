module Protocol where

import Data.List.Extra
import Control.Arrow

import Data
import Parser
import Solve
import Builtins
import Send

draw1 :: [(Integer,Integer)] -> String
draw1 xs = if null xs then "blank" else let
  xmin = fromIntegral $ foldr1 min (map fst xs)
  xmax = fromIntegral $ foldr1 max (map fst xs)
  ymin = fromIntegral $ foldr1 min (map snd xs)
  ymax = fromIntegral $ foldr1 max (map snd xs)
  srted = map ((snd.head &&& id) . sortOn fst) (groupSortOn snd xs)
  goFrom :: Integer -> Integer -> b -> (a -> b) -> [(Integer,a)] -> [b]
  goFrom mn mx blank fn [] = replicate (fromIntegral$ mx - mn) blank
  goFrom mn mx blank fn ((y,l):rs) = replicate (fromIntegral$ y - mn) blank ++ (fn l:
    goFrom (y+1) mx blank fn rs)
  strify = goFrom xmin (xmax+1) ' ' (const '#')
  in show (xmin,ymin)<>"\n"<>unlines( (map ( strify)) (goFrom ymin (ymax+1) [] id srted))


drawh :: [[(Integer,Integer)]] -> String
drawh xs = unlines (map draw1 xs)


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
