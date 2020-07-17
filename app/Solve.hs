{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Solve where

import Control.Lens hiding (List)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Foldable
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

import AST
import Data

helper :: [Statement] -> Map Opr Expr
helper sts = M.fromList $ [ (a ^?! _Operator, b) | Equivalence a b <- sts]

solve :: Opr -> Map Opr Expr -> Map Opr Value
solve v m =
  let -- eval precond : v and all deps exps present in expr map
      eval :: Opr -> State (Map Opr Value) Value
      eval v =
        (at v <?=) =<< evaluate (m ^?! ix v)
      evaluate :: Expr -> State (Map Opr Value) Value
      evaluate = \case
        Ap f e -> do
          f <- evaluate f <&> (^?! _VFunction)
          e <- evaluate e
          pure $ f e
        List es -> foldrM (\a b -> VCons <$> evaluate a ?? b) VNil es
        Operator op -> eval op
        Constant v -> pure $ VInt v
        Fn f -> pure $ predef f
   in flip execState M.empty $ eval v

----------------------
-- * Test
oprmap = M.fromList [(0, Constant 1), (2, List []), (3, Ap (Fn Inc) (Operator 0))]
oprmap2 = oprmap <> M.fromList [(4, Fn Dec), (5, Ap (Operator 4) (Operator 3))]

t = solve 3 oprmap
t2 = solve 5 oprmap2
