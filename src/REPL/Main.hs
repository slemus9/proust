module REPL.Main where

import System.IO ( stdout, hFlush )
import REPL.Command
import REPL.Parser ( parseCommand )
import Proust.PrettyPrinter

repl :: IO ()
repl = putStrLn "Proust REPL:" >> loop

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
processCommand (SetTask t)  = putStrLn "Setting task:" >> putStrLn (pprintType t) >> loop
processCommand (Refine n e) = putStrLn ("Refining hole ?" ++ show n) >> putStrLn (pprintExpr e) >> loop
processCommand EmptyLn      = loop
processCommand Quit         = putStrLn "Exiting ..."