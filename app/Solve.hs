{-# LANGUAGE LambdaCase #-}

module Solve
  (
  )
where

import Control.Lens hiding (List)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Foldable
import qualified Data.Map as M
import Data.Map (Map)

import AST

solve :: Var -> Map Var Expr -> Map Var Val
solve v m =
  let -- eval precond : v and all deps exps present in expr map
      eval :: Var -> State (Map Var Val) Val
      eval v =
        (at v <?=) =<< evaluate (m ^?! ix v)
      evaluate :: Expr -> State (Map Var Val) Val
      evaluate = \case
        Variable v -> eval v
        Constant v -> pure v
        _ -> error "undefined"
   in flip execState M.empty $ eval v

getDeps :: Expr -> [Var]
getDeps = \case
  Ap e1 e2 -> getDeps e1 <> getDeps e2
  List es -> concatMap getDeps es
  Variable v -> pure v
  Constant _ -> []
  Fn _ -> []
