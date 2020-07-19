{-# LANGUAGE TupleSections #-}

import Control.Exception
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Control.Applicative
import Control.Exception
import Data.Bifunctor
import GHC.IO
import Network.HTTP.Simple
import System.Environment
import System.IO
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

iterPoint :: Bool -> (Handle, Handle) -> (Value -> Value) -> History -> Value -> IO ()
iterPoint usePy pipes f (s:ss) p = do
  (ns, dat) <- alienInteract f (fst s) p
  -- print dat
  let (Just (x,y)) = detectCross' dat
  let nss = if s == (ns,dat) then (s:ss) else ((ns,dat):s:ss)
  print (x,y)
  (if usePy then runPython else ui) pipes (x,y) nss (\up ss' p -> iterPoint up pipes f ss' (uncurry mkPair p))

type Point = (Integer,Integer)

ui :: (Handle, Handle) -> Point -> History -> (Bool -> History -> Point -> IO ()) -> IO ()
ui pipes p@(x,y) s@((st,dat):_) f = do
  print (x,y)
  -- putStr (draw p dat)
  print st
  print (pp st)
  putStrLn "(q)uit, (o)utput, (l)oad"
  putStr "awaiting input (type 1 character (wasd (p)rint (r)un p(y)thon (b)ack) then press enter): "
  hFlush stdout
  inp <- getLine
  case head inp of
    'w' -> ui pipes (x,y - 1) s f
    's' -> ui pipes (x,y + 1) s f
    'a' -> ui pipes (x - 1,y) s f
    'd' -> ui pipes (x + 1,y) s f
    'p' -> ui pipes p s f
    'r' -> f False s p
    'y' -> runPython pipes p s f
    'b' -> ui pipes p (tail s) f
    'o' -> output p s *> ui pipes p s f
    'l' -> load >>= flip (uncurry (ui pipes)) f
    'e' -> do
      case parseJustExpr (tail inp) of
        Just e -> do
           let v = solve' 0 (M.singleton 0 e)
           --f False ((v,VNil):s) (0,0)
           ui pipes p ((v,VNil):s) f
           -- ui p _ f
        Nothing -> print "faild parse" >> ui pipes p s f

output :: Point -> History -> IO ()
output p s = do
  putStrLn "Enter file name to save:"
  fn <- ("saves/" <>) <$> getLine
  writeFile fn $ show (p, map (bimap serialize serialize) s)

load :: IO (Point, History)
load = do
  putStrLn "Enter saved filename:"
  fn <- ("saves/" <>) <$> getLine
  second (map $ bimap deserialize deserialize) . read <$> readFile fn

readPoint :: String -> Maybe Point
readPoint s = case break (== ' ') s of
      (a,(' ':b)) -> liftA2 (,) (readMaybe a) (readMaybe b)
      _ -> Nothing

runPython :: (Handle, Handle) -> Point -> History -> (Bool -> History -> Point -> IO ()) -> IO ()
runPython pipes@(outPipe, inPipe) p s@((_,dat):_) f = do
  hPutStrLn outPipe $ unlines $ annotations dat ++ reverse (listify3 dat)
  hFlush outPipe
  pt <- hGetLine inPipe
  case pt of
    [] -> ui pipes p s f
    'b':_ -> runPython pipes p (tail s) f
    'g':_ -> f True [(defaultAddress,VNil)] p
    't':_ -> ui pipes p s f
    _ -> case readPoint pt of
      Just p -> f True s p
      Nothing -> putStrLn ("Bad Python output: "++pt) >> ui pipes p s f

defaultAddress, galaxyAddress, ticTacToeEndAddress :: Value
defaultAddress = galaxyAddress
galaxyAddress = deserialize "11011000011111011010110011010110000"
ticTacToeEndAddress = deserialize "110110001111110101111011000101101011011000011101100001110110001011011000101101100001110110001011011000010011110111111100110100010101101001000011101111111001101000101101100111101111011111110011010001011011010000001110111111100111001100110010011000011101111111001110011001110100110101111011111110011100110011101111000001110111111100111001101110010101100011101111111001110101001111100001000111011111110101111101010000101100001110111111101011111010100001100010111101111111011000000011010111001101100110100011010110000"

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
        (Just outputPipe, Just inputPipe, _, _) <- createProcess (CreateProcess{
            cmdspec=RawCommand "python3" ["gridselect.py"],
            cwd = Nothing,
            env = Nothing,
            std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = Inherit,
            close_fds = False,
            create_group = False,
            delegate_ctlc = False,
            detach_console = False,
            create_new_console = False,
            new_session = False,
            child_group = Nothing,
            child_user = Nothing,
            use_process_jobs = False
          })
        iterPoint True (outputPipe, inputPipe) f [(defaultAddress,VNil)] inp

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

annotations :: Value -> [String]
annotations dat = map (\(x,y,w,h,s) -> unwords [show x, show y, show w, show h, s]) $ concatMap (annotations' . map vPair . toList) (toList dat)

annotations' :: [(Integer, Integer)] -> [(Integer, Integer, Integer, Integer, String)]
annotations' ps = flip mapMaybe ps $ (\(x0,y0') -> do
    let y0 = y0' + 1
    if S.member (x0-1, y0-1) img then Nothing else Just ()
    let topBorderLength  = fromIntegral $ length $ takeWhile (flip S.member img) $ map (\n -> (x0+n,y0-1)) [0..]
    let leftBorderLength = fromIntegral $ length $ takeWhile (flip S.member img) $ map (\n -> (x0-1,y0+n)) [0..]
    let bits = reverse $ (\y x -> if S.member (x+x0,y+y0) img then '1' else '0') <$> [0..topBorderLength-1] <*> [0..topBorderLength-1]
    let absValue = convertFromBits bits
    let requireEmpty p = if S.member p img then Nothing else Just ()
    sequence $ map (\n -> requireEmpty (n+x0, y0-2) >> requireEmpty (n+x0, y0+leftBorderLength) >> requireEmpty (x0-2, n+y0) >> requireEmpty (x0+topBorderLength, n+y0)) [(-2)..topBorderLength]
    (x0-1, y0-1, topBorderLength+1, leftBorderLength+1,) <$> case leftBorderLength - topBorderLength of
      0 -> Just (show absValue)
      1 -> Just (show (-absValue))
      _ -> Nothing
  )
  where img = S.fromList ps
