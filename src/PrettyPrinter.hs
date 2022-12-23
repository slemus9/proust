module PrettyPrinter where

import Language

pprintExpr :: Expr -> String
pprintExpr (Var x)     = pprintLit x

pprintExpr (Lam x e)   = '\\' : pprintLit x ++ " => " ++ pprintExpr e

pprintExpr (App e1 e2) = pr1 (pprintExpr e1) ++ " " ++ pr2 (pprintExpr e2)
  where
    pr1 = parenIf $ isNestedExpr e1 && not (isApp e1)
    pr2 = parenIf $ isNestedExpr e2

pprintExpr (Ann e t)   = pr (pprintExpr e) ++ " : " ++ pprintType t
  where
    pr = parenIf $ isNestedExpr e

pprintLit :: Literal -> String
pprintLit (Literal s) = s

pprintType :: TypeAnn -> String
pprintType (TypeVar t)   = pprintLit t
pprintType (Arrow t1 t2) = pr (pprintType t1) ++ " -> " ++ pprintType t2
  where
    pr = parenIf $ isNestedType t1

parenIf :: Bool -> (String -> String)
parenIf True  = wrapInParen
parenIf False = id

wrapInParen :: String -> String
wrapInParen s = '(' : s ++ ")"

isNestedType :: TypeAnn -> Bool
isNestedType (TypeVar _) = False
isNestedType _           = True

isNestedExpr :: Expr -> Bool
isNestedExpr (Var _) = False
isNestedExpr _       = True

isApp :: Expr -> Bool
isApp (App _ _) = True
isApp _         = False