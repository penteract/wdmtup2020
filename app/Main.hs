import Control.Exception
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Network.HTTP.Simple
import System.Environment
import GHC.IO
import System.IO

import Data
import Parser
import Solve
import Protocol

mkPair :: Integer -> Integer -> Value
mkPair x y = VCons (VInt $ x) (VInt $ y)

iterPoint f s p = do
  (ns, dat) <- alienInteract f s p
  -- print dat
  let (Just (x,y)) = detectCross' dat
  print (x,y)
  ui dat (x,y) (iterPoint f ns . uncurry mkPair)

type Point = (Integer,Integer)

ui :: Value -> Point -> (Point -> IO ()) -> IO ()
ui dat p@(x,y) f = do
  print (x,y)
  putStr (draw p dat)
  putStr "awaiting input (type 1 character (wasd (p)rint (r)un) then press enter):"
  hFlush stdout
  inp <- getLine
  let distance = fromIntegral $ length inp
  case head inp of
    'w' -> ui dat (x,y - distance) f
    's' -> ui dat (x,y + distance) f
    'a' -> ui dat (x - distance,y) f
    'd' -> ui dat (x + distance,y) f
    'p' -> ui dat p f
    'r' -> f p

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
        iterPoint f VNil inp

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
