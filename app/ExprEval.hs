module ExprEval where

import AST

-- Evaluate expressions to other expressions, with the restriction on the output that they don't contain lists or operators of known value with enough arguments that they could be evaluated, except in certain positions where maintaining laziness is desired. This is intended as an alternative way of representing values.
exprEvaluate :: Expr -> IO Expr
exprEvaluate (Ap f x) = flip exprApply x =<< exprEvaluate f
exprEvaluate (List xs) = listExpr <$> mapM exprEvaluate xs
exprEvaluate (Operator op) = pure $ Operator op -- TODO: Add a context to retrieve the value from.
exprEvaluate (Constant n) = pure $ Constant n
exprEvaluate (Fn Vec) = pure $ Fn Cons
exprEvaluate (Fn p) = pure $ Fn p

exprApply :: Expr -> Expr -> IO Expr
exprApply (Ap (Fn S) x0) x1 = pure (Ap (Ap (Fn S) x0) x1) -- maintain laziness.
exprApply (Ap (Fn C) x0) x1 = pure (Ap (Ap (Fn C) x0) x1)
exprApply (Ap (Fn B) x0) x1 = pure (Ap (Ap (Fn B) x0) x1)
exprApply (Fn F) x0 = pure (Ap (Fn F) x0)
exprApply (Ap (Fn If0) (Constant 1)) x0 = pure (Ap (Ap (Fn If0) (Constant 1)) x0)
exprApply (Ap (Ap (Fn S) x0) x1) x2 = do
  a <- exprApply x0 x2
  exprApply a (Ap x1 x2)
exprApply (Ap (Ap (Fn C) x0) x1) x2 = flip exprApply x1 =<< exprApply x0 x2
exprApply (Ap (Ap (Fn B) x0) x1) x2 = exprApply x0 (Ap x1 x2)
exprApply (Ap (Fn T) x0) x1 = exprEvaluate x0
exprApply (Ap (Fn F) x0) x1 = exprEvaluate x1
exprApply (Fn Nil) x = pure $ Fn T
exprApply (Ap (Ap (Fn If0) (Constant 0)) x0) x1 = pure $ x0
exprApply (Ap (Ap (Fn If0) (Constant 1)) x0) x1 = pure $ x1
exprApply f x = exprApply' f =<< exprEvaluate x

-- like exprApply, but assumes that its second argument is in normal form already.
exprApply' :: Expr -> Expr -> IO Expr
exprApply' (Fn Inc) (Constant n) = pure $ Constant (n+1)
exprApply' (Fn Dec) (Constant n) = pure $ Constant (n-1)
exprApply' (Ap (Fn Add) (Constant n)) (Constant n') = pure $ Constant (n+n')
exprApply' (Ap (Fn Mul) (Constant n)) (Constant n') = pure $ Constant (n*n')
exprApply' (Ap (Fn Div) (Constant n)) (Constant n') = pure $ Constant (quot n n')
exprApply' (Ap (Fn Eq) x) y = pure $ boolExpr (x == y)
exprApply' (Ap (Fn Lt) (Constant n)) (Constant n') = pure $ boolExpr (n < n')
exprApply' (Fn Mod) x = pure x
exprApply' (Fn Dem) x = pure x
--exprApply (Fm Send) x = 
exprApply' (Fn Neg) (Constant n) = pure $ Constant (-n)
exprApply' (Fn Pwr2) (Constant n) = pure $ Constant (2^n)
exprApply' (Fn I) x = pure $ x
exprApply' (Ap (Ap (Fn Cons) x0) x1) x2 = flip exprApply x1 =<< exprApply x2 x0
exprApply' (Fn Car) x = exprApply x (Fn T)
exprApply' (Fn Cdr) x = exprApply x (Fn F)
exprApply' (Fn Isnil) (Fn Nil) = pure $ Fn T
exprApply' (Fn Isnil) (Ap (Ap (Fn Cons) x0) x1) = pure $ Fn F
exprApply' (Fn Draw) x = pure $ Picture
exprApply' (Ap (Fn Checkerboard) (Constant n)) (Constant n') = pure $ Picture
exprApply' (Fn Multipledraw) (Fn Nil) = pure $ Fn Nil
exprApply' (Fn Multipledraw) (Ap (Ap (Fn Cons) x0) x1) = do
  p0 <- exprApply (Fn Draw) x0
  p1 <- exprApply (Fn Multipledraw) x1
  pure $ Ap (Ap (Fn Cons) p0) p1
exprApply' (Fn Modem) x = exprApply (Fn Dem) =<< exprApply (Fn Mod) x
exprApply' (Ap (Fn F38) protocol) (Ap (Ap (Fn Cons) (Constant flag)) (Ap (Ap (Fn Cons) newState) (Ap (Ap (Fn Cons) theData) (Fn Nil)))) = if flag == 0
  then listExpr <$> sequence [exprApply (Fn Modem) newState, exprApply (Fn Multipledraw) theData]
  else do
    withProtocol <- exprApply (Fn Interact) protocol
    modemedState <- exprApply (Fn Modem) newState
    withState <- exprApply withProtocol modemedState
    sentData <- exprApply (Fn Send) theData
    exprApply withState sentData
exprApply' (Ap (Ap (Fn Interact) x2) x4) x3 = do
  withProtocol <- exprApply (Fn F38) x2
  withState <- exprApply x2 x4
  withPixel <- exprApply withState x3
  exprApply withProtocol withPixel
-- Statelessdraw and Statelessdraw not implemented.
exprApply' f x = pure $ Ap f x

boolExpr :: Bool -> Expr
boolExpr True  = Fn T
boolExpr False = Fn F

listExpr :: [Expr] -> Expr
listExpr = foldr (\x y -> Ap (Ap (Fn Cons) x) y) (Fn Nil)
