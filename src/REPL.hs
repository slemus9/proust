module REPL (repl) where

import System.IO ( stdout, hFlush )
import Text.Parsec
import Language
import Data.Functor ( ($>) )
import Data.Functor.Identity ( Identity )
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P
import qualified Parser as ExprParser

repl :: IO ()
repl = print "Proust REPL:" >> loop

loop :: IO ()
loop = do
  putStr "> "
  hFlush stdout
  getLine >>= processLine

processLine :: String -> IO ()
processLine s = case parseCommand s of
  Right cmd -> processCommand cmd
  Left e    -> print e >> loop

processCommand :: Command -> IO ()
processCommand (SetTask t) = print "Setting task:" >> print t >> loop
processCommand EmptyLn     = loop
processCommand Quit        = print "Exiting ..."

data Command = 
    SetTask TypeAnn
  | EmptyLn
  | Quit
  deriving Show

-- Parser definition
langDef :: P.LanguageDef st
langDef = P.emptyDef
  { P.reservedOpNames = ["set-task!", "quit!"] }

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
command = choice [quit, setTask, emptyLn]

setTask :: Parsec String st Command
setTask = SetTask <$> (reservedOp "set-task!" *> parseType)
  where 
    parseType = label ExprParser.typeAnn "expecting a type annotation"

quit :: Parsec String st Command
quit = reservedOp "quit!" $> Quit

emptyLn :: Parsec String st Command
emptyLn = (whiteSpace >> notFollowedBy anyToken) $> EmptyLn