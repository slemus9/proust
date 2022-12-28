module TypeCheck where

import qualified Data.Map.Strict as M
import Language
import Data.Functor ( ($>) )

type Context = M.Map Literal TypeAnn

data TypeCheckError =
    CannotCheck Context Expr TypeAnn
  | CannotSynth Context Expr
  deriving Show

type TypeCheckResult = Either TypeCheckError TypeAnn

-- Type checking
typeCheck :: Context -> Expr -> TypeAnn -> TypeCheckResult
-- Implication introduction rule
typeCheck ctx (Lam x e) (Arrow t1 t2) = 
  typeCheck (M.insert x t1 ctx) e t2

-- A lambda should have a type of the form (Arrow t1 t2)
typeCheck ctx e @ (Lam _ _) t = Left $ CannotCheck ctx e t

-- We switch to type synthesis mode if there are no more rules that apply
typeCheck ctx e t = do
  t' <- typeSynth ctx e
  if t == t' then 
    Right t
  else 
    Left $ CannotCheck ctx e t

-- Type synthesis
typeSynth :: Context -> Expr -> TypeCheckResult
-- We have no rules for synthesizing a lambda
typeSynth ctx e @ (Lam _ _) = Left $ CannotSynth ctx e

-- We switch to type check mode if there is a type annotation
typeSynth ctx (Ann e t)     = typeCheck ctx e t

{-
  To synthesize the type of implication elimination (modus ponens),
  we synthesize/infer the type of the function, and we check type check
  the argument according to the type of the our function
-}
typeSynth ctx e @ (App f a) = do
  tf <- typeSynth ctx f
  case tf of
    Arrow t1 t2 -> typeCheck ctx a t1 $> t2
    TypeVar _   -> Left $ CannotSynth ctx e

-- To synthesize the type of a variable, we lookup the variable in our type context
typeSynth ctx e @ (Var x)      
  | Just t <- M.lookup x ctx = Right t
  | otherwise                = Left $ CannotSynth ctx e