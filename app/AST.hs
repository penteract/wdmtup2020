{-# LANGUAGE TemplateHaskell #-}

module AST where

import Control.Lens
import Data

data Statement = Equivalence Expr Expr deriving (Show)

type Opr = Integer

type Val = Integer

data Predef
  = Inc
  | Dec
  | Add
  | Mul
  | Div
  | Eq
  | Lt
  | Mod
  | Dem
  | Send
  | Neg
  | S
  | C
  | B
  | T
  | F
  | Pwr2
  | I
  | Cons
  | Car
  | Cdr
  | Nil
  | Isnil
  | Vec
  | Draw
  | Checkerboard
  | Multipledraw
  | If0
  | Interact
  | Modem
  | F38
  | Statelessdraw
  | Statefuldraw
  deriving (Bounded, Enum, Show, Eq, Ord)

data Expr = Ap Expr Expr | List [Expr] | Operator Opr | Constant Val | Fn Predef | Picture deriving (Show, Eq)

makePrisms ''Expr
