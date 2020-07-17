module Parser
where

import AST

parse :: String -> [Statement]
parse s = (unlines s >>= (maybe [] . parseLine) )


parseLine :: String -> Maybe Statement
parseLine = parseLine

parseExpr :: [String] -> (Expr, [String])
parseExpr = parseExpr
