module Parser
where

import AST
import System.IO.Unsafe
import qualified Data.Map as M
import Data.Char(toLower)

parse :: String -> [Statement]
parse s = (init (lines s) >>= (maybe [] return . parseLine) )


defStrings :: M.Map String Predef
defStrings = M.fromList [ (map toLower (show x), x) | x <- [minBound .. maxBound]]

type Token = String

tokenise :: String -> [Token]
tokenise [] = []
tokenise (' ':rs) = tokenise rs
tokenise (c:rs) = let (h,rs') = tokenise' [c] rs in (h:tokenise rs')

tokenise' start [] = (reverse start,[])
tokenise' start (' ':rs) = (reverse start, rs)
tokenise' start (c:rs) = tokenise' (c:start) rs

parseLine :: String -> Maybe Statement
parseLine l = let toks = tokenise l in
  case toks of
    (h:"=":rs) -> do
      (lhs,[]) <- parseExpr [h]
      (rhs,[]) <- parseExpr rs
      return (Equivalence lhs rhs)
    _ -> error "bad line"

isGood _ = True

toConst (':':rs) = Operator (read rs)
toConst ('-':rs) = Constant (negate $ read rs)
toConst (name@(c:rs)) = if c `elem` ['0'..'9'] then Constant (read (c:rs))
  else if M.member name defStrings then Fn (defStrings M.! name)
    else error ("unknown name: "++name)

parseJustExpr :: String -> Maybe Expr
parseJustExpr s = let toks = tokenise s in
  case toks of
    (rs) -> do
      (rhs,[]) <- parseExpr rs
      return (rhs)
    _ -> Nothing

parseExpr :: [Token] -> Maybe (Expr, [Token])
parseExpr ("ap":rs) = do
  (x, rs) <- parseExpr rs
  (y, rs) <- parseExpr rs
  return (Ap x y, rs)
parseExpr ("(":rs) = parseList [] rs
parseExpr (tok:rs) = return (toConst tok, rs)
parseExpr [] = Nothing --error "bad expr"


-- not the usual parser, but hey
parseList :: [Expr] -> [Token] -> Maybe (Expr,[Token])
parseList start (")":rs) = return (List (reverse start), rs)
parseList start (",":rs) = parseList start rs
parseList start [] = Nothing -- error "bad list"
parseList start rs = do
  (x,rs) <- parseExpr rs
  parseList (x:start) rs
