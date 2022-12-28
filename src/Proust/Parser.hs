module Proust.Parser where

import Text.Parsec
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import Data.Functor.Identity
import Proust.Language

-- Parser definition
langDef :: P.LanguageDef st
langDef = P.emptyDef
  { P.reservedOpNames = reservedOperations }

-- Parse function
parseExpr :: String -> Either ParseError Expr
parseExpr = parse (whiteSpace *> expr) ""

-- Parser definitions
lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser langDef

parens :: Parsec String st a -> Parsec String st a
parens = P.parens lexer

identifier :: Parsec String st String
identifier = P.identifier lexer

reservedOp :: String -> Parsec String st ()
reservedOp = P.reservedOp lexer

whiteSpace :: Parsec String st ()
whiteSpace = P.whiteSpace lexer

-- Expr
lit :: Parsec String st Literal
lit = Literal <$> identifier

var :: Parsec String st Expr
var = Var <$> lit

lambda :: Parsec String st Expr
lambda = do
  reservedOp "\\"
  x <- lit
  reservedOp "=>"
  Lam x <$> expr

aExpr :: Parsec String st Expr
aExpr = var <|> parens expr

apply :: Parsec String st Expr
apply = foldl1 App <$> many1 aExpr -- Left associative

expr :: Parsec String st Expr
expr = joinExpr <$> choice [lambda, apply] <*> optionalType
  where
    optionalType = optionMaybe (reservedOp ":" *> typeAnn)
    joinExpr e Nothing  = e
    joinExpr e (Just t) = Ann e t

-- Type
typeVar :: Parsec String st TypeAnn
typeVar = TypeVar <$> lit

aType :: Parsec String st TypeAnn
aType = typeVar <|> parens typeAnn

typeAnn :: Parsec String st TypeAnn
typeAnn = foldr1 Arrow <$> types -- Right associative
  where
    types = sepBy1 aType (reservedOp "->")