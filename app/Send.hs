module Send where
import Control.Exception
import Data
import Data.ByteString.Lazy.UTF8 as BLU
import Network.HTTP.Simple
import System.Environment

import AST
import Data

-- send data to the server and recieve a reply
sendTo :: String -> Value -> IO Value
sendTo url v = do
    request' <- parseRequest ("POST " ++ url)
    let request = setRequestBodyLBS (BLU.fromString (serialize v)) request'
    response <- httpLBS request
    let statuscode = show (getResponseStatusCode response)
    case statuscode of
      "200" -> do
         let s = BLU.toString (getResponseBody response)
         putStrLn ("Server response: " ++ s)
         return (deserialize s)
      _ -> error ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))

send = sendTo "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=7897f34898d14e438f654b62eb7f8673"
