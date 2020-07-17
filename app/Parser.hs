module Parser
where

import AST

parse :: String -> [Statement]
parse s = (lines s >>= (maybe [] return . parseLine) )


parseLine :: String -> Maybe Statement
parseLine = parseLine

parseExpr :: [String] -> (Expr, [String])
parseExpr = parseExpr
