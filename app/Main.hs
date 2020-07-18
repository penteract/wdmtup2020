import Control.Exception
import Data
import Data.ByteString.Lazy.UTF8 as BLU
import Network.HTTP.Simple
import System.Environment

import Data
import Parser
import Solve
import Builtins

main =
  catch
    ( do
        args <- getArgs
        if Prelude.length args < 2
          then putStrLn "Arguments required: URL to send to (including key: https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=7897f34898d14e438f654b62eb7f8673) and message to send."
          else do
            file <- readFile "galaxy.txt"
            let vals = parse file
            let (VFunction f) = solve' 1338 (helper $ vals)
            let x = (apply (f (VNil)) (VCons (VInt 0) (VInt 0)))
            print x
            print (serialize (toList x !! 2))
            -- server contact

            request' <- parseRequest ("POST " ++ (args !! 0))
            let request = setRequestBodyLBS (BLU.fromString (args !! 1)) request'
            response <- httpLBS request
            let statuscode = show (getResponseStatusCode response)
            case statuscode of
              "200" -> putStrLn ("Server response: " ++ (show $ deserialize $ BLU.toString $ getResponseBody response))
              _ -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
    )
    handler
  where
    handler :: SomeException -> IO ()
    handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex
