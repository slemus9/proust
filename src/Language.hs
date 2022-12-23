module Language where

data Expr =
    Var Literal
  | Lam Literal Expr -- Lambda expression, \a => b
  | App Expr Expr    -- Function application, f a 
  | Ann Expr TypeAnn -- Type annotation, t : T
  deriving (Show, Eq)

data TypeAnn =
    Arrow TypeAnn TypeAnn -- Function type, A -> B
  | TypeVar Literal       -- A type, T
  deriving (Show, Eq)

newtype Literal = Literal String
  deriving (Show, Eq)

-- Reserved symbols
reservedOperations :: [String]
reservedOperations = ["\\", "=>", "->", ":"]