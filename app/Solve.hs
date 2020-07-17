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

solve :: Opr -> Map Opr Expr -> Map Opr Value
solve v m =
  let -- eval precond : v and all deps exps present in expr map
      eval :: Opr -> State (Map Opr Value) Value
      eval v =
        (at v <?=) =<< evaluate (m ^?! ix v)
      evaluate :: Expr -> State (Map Opr Value) Value
      evaluate = \case
        Ap f e -> do
          f' <- (^?! _VFunction) <$> evaluate f
          e' <- evaluate e
          pure $ f' e'
        List es -> foldrM (\a b -> VCons <$> evaluate a <*> pure b) VNil es
        Operator op -> eval op
        Constant v -> pure $ VInt v
        Fn f -> pure $ predef f
   in flip execState M.empty $ eval v

getDeps :: Expr -> [Opr]
getDeps = \case
  Ap e1 e2 -> getDeps e1 <> getDeps e2
  List es -> concatMap getDeps es
  Operator op -> pure op
  Constant _ -> []
  Fn _ -> []

----------------------
-- * Test
oprmap = M.fromList [(0, Constant 1), (2, List []), (3, Ap (Fn Inc) (Operator 0))]

t = solve 3 oprmap
