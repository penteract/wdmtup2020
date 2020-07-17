module AST where

data Statement = Equivalence Expr Expr

data Expr = Ap Expr Expr | List [Expr] | Variable Integer | Constant Integer | Fn String
