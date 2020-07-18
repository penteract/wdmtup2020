import Control.Exception
import Data.ByteString.Lazy.UTF8 as BLU
import Network.HTTP.Simple
import System.Environment

import Data
import Parser
import Solve
import Builtins
import Send



-- Takes [[(Int,Int)]] as values and returns a sequence of images
draw :: Value -> String
draw s = ("to be drawn: "++show s)


alienInteract :: (Value -> Value) -> Value -> Value -> IO ()
alienInteract f state vec = do
  let (flag:newState:dat:_) = toList (apply (f (VNil)) (VCons (VInt 0) (VInt 0)))
  if flag==(VInt 0) then do
    putStrLn "Terminate Interaction"
    print newState
    putStrLn (draw dat)
  else do
    putStrLn "Continue Interaction"
    print newState
    resp <- send dat
    alienInteract f newState resp



main =
  catch
    ( do
        file <- readFile "galaxy.txt"
        let vals = parse file
        let (VFunction f) = solve' 1338 (helper $ vals)
        alienInteract f VNil (VCons (VInt 0) (VInt 0))
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
