import Control.Exception
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Network.HTTP.Simple
import System.Environment
import GHC.IO
import System.IO
import Control.Applicative
import System.Process

import Text.Read
import Data
import Parser
import Solve
import Protocol

mkPair :: Integer -> Integer -> Value
mkPair x y = VCons (VInt $ x) (VInt $ y)

-- The state, then the list of images.
type History = [(Value, Value)]

iterPoint :: (Value -> Value) -> History -> Value -> IO ()
iterPoint f (s:ss) p = do
  (ns, dat) <- alienInteract f (fst s) p
  -- print dat
  let (Just (x,y)) = detectCross' dat
  let nss = if s == (ns,dat) then (s:ss) else ((ns,dat):s:ss)
  print (x,y)
  ui (x,y) nss (\ss' p -> iterPoint f ss' (uncurry mkPair p))

type Point = (Integer,Integer)

ui :: Point -> History -> (History -> Point -> IO ()) -> IO ()
ui p@(x,y) s@((_,dat):_) f = do
  print (x,y)
  putStr (draw p dat)
  putStr "awaiting input (type 1 character (wasd (p)rint (r)un) then press enter):"
  hFlush stdout
  inp <- getLine
  let distance = fromIntegral $ length inp
  case head inp of
    'w' -> ui (x,y - distance) s f
    's' -> ui (x,y + distance) s f
    'a' -> ui (x - distance,y) s f
    'd' -> ui (x + distance,y) s f
    'p' -> ui p s f
    'r' -> f s p
    'y' -> runPython p s f
    'b' -> ui p (tail s) f

readPoint :: String -> Maybe Point
readPoint s = case break (== ' ') s of
      (a,(' ':b)) -> liftA2 (,) (readMaybe a) (readMaybe b)
      _ -> Nothing

runPython :: Point -> History -> (History -> Point -> IO ()) -> IO ()
runPython p s@((_,dat):_) f = do
  pt <- readProcess "python3" ["gridselect.py"] (unlines (listify3 dat))
  case readPoint pt of
    Just p -> f s p
    Nothing -> putStrLn "Bad Python output" >> ui p s f


main =
  catch
    ( do
        file <- readFile "galaxy.txt"
        let vals = parse file
        let (VFunction f) = solve' 1338 (helper $ vals)
        args <- getArgs
        print args
        -- let istate = if Prelude.length args == 2 then VCons (VInt $ read (args!! 0)) (VInt $ read (args !! 1)) else (VCons (VInt 0) (VInt 0))
        let inp = if Prelude.length args == 2
                        then mkPair (read$ args!! 0) (read $ (args !! 1))
                        else (VCons (VInt 0) (VInt 0))
        iterPoint f [(VNil,VNil)] inp

         --(VCons (VInt 0) (VInt 0))
        -- let x = (apply (f (VNil)) (VCons (VInt 0) (VInt 0)))
        -- print x
        -- print (serialize (toList x !! 2))



            {-stuff that might be helpful later for working with the docker thing
            See https://github.com/icfpcontest2020/starterkit-haskell/blob/master/app/Main.hs
        args <- getArgs
        if Prelude.length args < 2
          then putStrLn "Arguments required: URL to send to (including key: https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=7897f34898d14e438f654b62eb7f8673) and message to send."
          else
            request' <- parseRequest ("POST " ++ (args !! 0))
            let request = setRequestBodyLBS (BLU.fromString (args !! 1)) request'
            response <- httpLBS request
            let statuscode = show (getResponseStatusCode response)
            case statuscode of
              "200" -> putStrLn ("Server response: " ++ (show $ deserialize $ BLU.toString $ getResponseBody response))
              _ -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
            -}
    )
    handler
  where
    handler :: SomeException -> IO ()
    handler ex = putStrLn $ "Predicatble error:\n" ++ show ex
