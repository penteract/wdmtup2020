module Send where
import Control.Exception
import Data
import Data.ByteString.Lazy.UTF8 as BLU
import Network.HTTP.Simple
import System.Environment

import AST
import Data

-- send data to the server and recieve a reply
send :: Value -> IO Value
send v = undefined {-
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
-}
