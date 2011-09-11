module Main where

import Brizo.Language
import Brizo.Parser
import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Exit
import System.IO
import Text.Parsec

main = do putStrLn "Brizo, a toy interpreter by Spencer Gordon."
          repl
          putStrLn "Bye. See you soon..."
          


prompt :: IO String
prompt = do putStr "> "
            hFlush stdout
            input <- getLine
            return input

{--
type EvaluationContext = ReaderT Frame Identity

data EvaluationError = EmptyInputError
                     | UndefinedSymbolError Symbol
                     | SyntaxError String
                     | ArgumentTypeError Expression
                     | OtherEvaluationError String
                       
instance Show EvaluationError where
  show EmptyInputError = "No input was provided."
  show (UndefinedSymbolError s) = "The following symbol was undefinded: " ++ (show s)
  show (SyntaxError s) = "Malformed syntax: " ++ s
  show (ArgumentTypeError e) = "Arguments didn't match the function being applied in: " ++ (show e)
  show (OtherEvaluationError s) = "Evaluation failed: " ++ s
                       
class Error EvaluationError where
  noMsg = OtherEvaluationError "other unspecified error"
  strMsg = OtherEvaluationError
                       
type Evaluation = Either EvaluationError
--}

repl :: IO ()
repl = do input <- prompt
          case processInput input of 
            Action action -> do action
            Evaluate expression -> do print expression
            InputError parseError -> do print parseError
          repl

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
interpreterActions = [ ("quit", exitWith ExitSuccess)
                     , ("help", putStrLn "Printing help values")
                     ]
                     
  