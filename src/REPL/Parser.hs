module REPL.Parser where

import REPL.Command
import Text.Parsec
import Data.Functor ( ($>) )
import Data.Functor.Identity ( Identity )
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import qualified Proust.Parser as ExprParser

-- Parser definition
langDef :: P.LanguageDef st
langDef = P.emptyDef
  { P.reservedOpNames = ["!set-task", "!quit", "!refine"] }

-- Parse function
parseCommand :: String -> Either ParseError Command
parseCommand = parse command ""

-- Parser definitions
lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser langDef

reservedOp :: String -> Parsec String st ()
reservedOp = P.reservedOp lexer

whiteSpace :: Parsec String st ()
whiteSpace = P.whiteSpace lexer

command :: Parsec String st Command
command = choice [quit, setTask, refine, emptyLn]

setTask :: Parsec String st Command
setTask = SetTask <$> (reservedOp "!set-task" *> parseType)
  where
    parseType = label ExprParser.typeAnn "expecting a type annotation"

int :: Parsec String st Int
int = read <$> many1 digit

refine :: Parsec String st Command
refine = do
  reservedOp "!refine"
  n <- int
  whiteSpace
  Refine n <$> ExprParser.expr

quit :: Parsec String st Command
quit = reservedOp "!quit" $> Quit

emptyLn :: Parsec String st Command
emptyLn = (whiteSpace >> notFollowedBy anyToken) $> EmptyLn