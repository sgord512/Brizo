module Main where

import Core.Base
import Core.Builtin
import Core.Evaluator
import Core.Parser
import Control.Monad.Error
import Control.Monad.State
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Console.Haskeline
import System.Exit
import System.IO
import Text.Parsec

main :: IO ()
main = do putStrLn "Brizo, a toy interpreter by Spencer Gordon."
          runInputT defaultSettings (repl $ allBuiltins:[])
          putStrLn "Bye. See you soon..."

reportResult :: Either EvaluationError Value -> InputT IO ()
reportResult (Left e) = outputStrLn $ "Error: " ++ (show e)
reportResult (Right v) = outputStrLn $ show v

repl :: Scope -> InputT IO ()
repl scope = do input <- getInputLine "> "
                case processInput input of 
                  Action (shouldQuit, action) -> do action
                                                    unless shouldQuit (repl scope)
                  Evaluate expression -> do let (result, modifiedScope) = runState (runErrorT $ evaluate expression) scope
                                            reportResult result
                                            repl modifiedScope
                  InputError parseError -> do outputStrLn $ show parseError
                                              repl scope
                  EmptyInput -> do outputStrLn "No input provided."
                                   repl scope

-- This is going to handle all input before it is evaluated, so that interpreter commands can be intercepted and handled appropriately, should use InputError Monad

processInput :: (Maybe String) -> InterpreterInput
processInput (Just str) = case lookup str interpreterActions of 
  Nothing -> case parse interpreterExpression "input" str of                 
    Left err -> InputError err
    Right exp -> Evaluate exp
  Just action -> Action action
processInput Nothing = EmptyInput

data InterpreterInput = Action InterpreterAction | Evaluate Expression | InputError ParseError | EmptyInput
type InterpreterAction = (Bool, InputT IO ())


-- Different actions defined as commands for the interpreter

interpreterActions :: [(String, InterpreterAction)]
interpreterActions = [ ("quit", (True, exit))
                     , ("help", (False, outputStrLn "Printing help values"))
                     ]
                     


exit = do outputStrLn "Bye. See you soon..."
  