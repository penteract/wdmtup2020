module AST where

data Statement = Equivalence Expr Expr

type Var = Integer
type Val = Integer

data Expr = Ap Expr Expr | List [Expr] | Variable Var | Constant Val | Fn String
