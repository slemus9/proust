module Language () where

import qualified Data.Text as T

data Expr =
    Lam Symbol Expr -- Lambda expression, \a => b
  | App Expr Expr   -- Function application, f a 
  | Ann Expr Type   -- Type annotation, t : T

data Type =
    Arrow Type Type -- Function type, A -> B
  | Type Symbol     -- A type, T

newtype Symbol = Symbol T.Text