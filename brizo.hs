module Main where

import Core.Base
import Core.Builtin
import Core.Evaluator
import Core.Language
import Core.Parser
import Control.Monad.Error
import Control.Monad.State
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Exit
import System.IO
import Text.Parsec

main = do putStrLn "Brizo, a toy interpreter by Spencer Gordon."
          repl (allBuiltins:[])
          putStrLn "Bye. See you soon..."
          


prompt :: IO String
prompt = do putStr "> "
            hFlush stdout
            input <- getLine
            return input

reportResult :: Either EvaluationError Value -> IO ()
reportResult (Left e) = putStrLn $ "Error: " ++ (show e)
reportResult (Right v) = print v

repl :: Scope -> IO ()
repl scope = do input <- prompt
                case processInput input of 
                  Action action -> do action
                                      repl scope
                  Evaluate expression -> do let (result, modifiedScope) = runState (runErrorT $ evaluate expression) scope
                                            reportResult result
                                            repl modifiedScope
                  InputError parseError -> do print parseError
                                              repl scope

-- This is going to handle all input before it is evaluated, so that interpreter commands can be intercepted and handled appropriately, should use InputError Monad

processInput :: String -> InterpreterInput
processInput str = case lookup str interpreterActions of 
  Nothing -> case parse interpreterExpression "input" str of                 
    Left err -> InputError err
    Right exp -> Evaluate exp
  Just action -> Action action
  
-- Evaluates an expression, very much incomplete, should use EvaluationError Monad 
{--
evaluateInContext :: Expression -> Frame -> Evaluation Expression
evaluateInContext exp bindings = case exp of
  Nil -> Nil
  (Cons s e) -> case s of
    (Sym s) -> maybe (throwError $ UndefinedSymbolError s) (\f -> apply f e) (Map.lookup s bindings)
    _ -> exp
  _ -> exp
--}
-- Types of valid input to the interpreter

data InterpreterInput = Action InterpreterAction | Evaluate Expression | InputError ParseError

-- Different actions defined as commands for the interpreter

interpreterActions :: [(String, InterpreterAction)]
interpreterActions = [ ("quit", exit)
                     , ("help", putStrLn "Printing help values")
                     ]
                     
  