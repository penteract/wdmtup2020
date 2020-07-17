module Parser
where

import AST
import System.IO.Unsafe

parse :: String -> [Statement]
parse s = (lines s >>= (maybe [] return . parseLine) )

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
  else if isGood name then Fn name else error "unknown name"
toConst [] = error "empty name"

parseExpr :: [Token] -> Maybe (Expr, [Token])
parseExpr ("ap":rs) = do
  (x, rs) <- parseExpr rs
  (y, rs) <- parseExpr rs
  return (Ap x y, rs)
parseExpr ("(":rs) = parseList [] rs
parseExpr (tok:rs) = return (toConst tok, rs)
parseExpr [] = error "bad expr"


-- not the usual parser, but hey
parseList :: [Expr] -> [Token] -> Maybe (Expr,[Token])
parseList start (")":rs) = return (List (reverse start), rs)
parseList start (",":rs) = parseList start rs
parseList start [] = error "bad list"
parseList start rs = do
  (x,rs) <- parseExpr rs
  parseList (x:start) rs
