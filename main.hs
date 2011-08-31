module Main where

import Brizo.Language
import Brizo.Parser
import System.Exit
import System.IO
import Text.Parsec

main = do putStrLn "Lem, a toy interpreter by Spencer Gordon."
          repl
          putStrLn "Bye. See you soon..."
          


prompt :: IO String
prompt = do putStr "> "
            hFlush stdout
            input <- getLine
            return input



repl :: IO ()
repl = do input <- prompt
          case processInput input of 
            Action action -> do action
            Evaluate expression -> do print expression
            InputError parseError -> do print parseError
          repl
            
                 

processInput :: String -> InterpreterInput
processInput str = case lookup str interpreterActions of 
  Nothing -> case parse expression "input" str of                 
    Left err -> InputError err
    Right exp -> Evaluate exp
  Just action -> Action action

evaluate :: Expression -> String
evaluate exp = error "This expression cannot yet be evaluated"

data InterpreterInput = Action InterpreterAction | Evaluate Expression | InputError ParseError

interpreterActions :: [(String, InterpreterAction)]
interpreterActions = [ ("quit", exitWith ExitSuccess)
                     , ("help", putStrLn "Printing help values")
                     ]