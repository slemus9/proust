module REPL.Command where

import Proust.Language

data Command =
    SetTask TypeAnn
  | Refine Int Expr
  | EmptyLn
  | Quit
  deriving Show