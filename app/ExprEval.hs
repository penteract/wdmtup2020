module ExprEval where

import AST

-- Evaluate expressions to other expressions, with the restriction on the output that they don't contain lists or operators of known value with enough arguments that they could be evaluated. This is intended as an alternative way of representing values.
exprEvaluate :: Expr -> Expr
exprEvaluate (Ap f x) = exprApply (exprEvaluate f) (exprEvaluate x)
exprEvaluate (List xs) = listExpr $ map exprEvaluate xs
exprEvaluate (Operator op) = Operator op -- TODO: Add a context to retrieve the value from.
exprEvaluate (Constant n) = Constant n
exprEvaluate (Fn Vec) = Fn Cons
exprEvaluate (Fn p) = Fn p

exprApply :: Expr -> Expr -> Expr
exprApply (Fn Inc) (Constant n) = Constant (n+1)
exprApply (Fn Dec) (Constant n) = Constant (n-1)
exprApply (Ap (Fn Add) (Constant n)) (Constant n') = Constant (n+n')
exprApply (Ap (Fn Mul) (Constant n)) (Constant n') = Constant (n*n')
exprApply (Ap (Fn Div) (Constant n)) (Constant n') = Constant (quot n n')
exprApply (Ap (Fn Eq) x) y = boolExpr (x == y)
exprApply (Ap (Fn Lt) (Constant n)) (Constant n') = boolExpr (n < n')
exprApply (Fn Mod) x = x
exprApply (Fn Dem) x = x
--exprApply (Fm Send) x = 
exprApply (Fn Neg) (Constant n) = Constant (-n)
exprApply (Ap (Ap (Fn S) x0) x1) x2 = exprApply (exprApply x0 x2) (exprApply x1 x2)
exprApply (Ap (Ap (Fn C) x0) x1) x2 = exprApply (exprApply x0 x2) x1
exprApply (Ap (Ap (Fn B) x0) x1) x2 = exprApply x0 (exprApply x1 x2)
exprApply (Ap (Fn T) x0) x1 = x0
exprApply (Ap (Fn F) x0) x1 = x1
exprApply (Fn Pwr2) (Constant n) = Constant (2^n)
exprApply (Fn I) x = x
exprApply (Ap (Ap (Fn Cons) x0) x1) x2 = exprApply (exprApply x2 x0) x1
exprApply (Fn Car) x = exprApply x (Fn T)
exprApply (Fn Cdr) x = exprApply x (Fn F)
exprApply (Fn Nil) x = Fn T
exprApply (Fn Isnil) (Fn Nil) = Fn T
exprApply (Fn Isnil) (Ap (Ap (Fn Cons) x0) x1) = Fn F
exprApply (Fn Draw) x = Picture
exprApply (Ap (Fn Checkerboard) (Constant n)) (Constant n') = Picture
exprApply (Fn Multipledraw) (Fn Nil) = (Fn Nil)
exprApply (Fn Multipledraw) (Ap (Ap (Fn Cons) x0) x1) = Ap (Ap (Fn Cons) (exprApply (Fn Draw) x0)) (exprApply (Fn Multipledraw) x1)
exprApply (Ap (Ap (Fn If0) (Constant 0)) x0) x1 = x0
exprApply (Ap (Ap (Fn If0) (Constant 1)) x0) x1 = x1
exprApply (Fn Modem) x = exprApply (Fn Dem) (exprApply (Fn Mod) x)
exprApply (Ap (Fn F38) protocol) (Ap (Ap (Fn Cons) (Constant flag)) (Ap (Ap (Fn Cons) newState) (Ap (Ap (Fn Cons) theData) (Fn Nil)))) = if flag == 0
  then listExpr [exprApply (Fn Modem) newState, exprApply (Fn Multipledraw) theData]
  else exprApply (exprApply (exprApply (Fn Interact) protocol) (exprApply (Fn Modem) newState)) (exprApply (Fn Send) theData)
exprApply (Ap (Ap (Fn Interact) x2) x4) x3 = exprApply (exprApply (Fn F38) x2) (exprApply (exprApply x2 x4) x3)
-- Statelessdraw and Statelessdraw not implemented.
exprApply f x = Ap f x

boolExpr :: Bool -> Expr
boolExpr True  = Fn T
boolExpr False = Fn F

listExpr :: [Expr] -> Expr
listExpr = foldr (\x y -> Ap (Ap (Fn Cons) x) y) (Fn Nil)
