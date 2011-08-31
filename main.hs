module Main where

import System.Exit
import System.IO

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
            Evaluate expression -> do let value = evaluate expression
                                      putStrLn (show value)
                                      repl
            
                 

processInput :: String -> InterpreterInput
processInput str = case lookup str interpreterActions of 
  Nothing -> Evaluate $ Result $ Concrete str
  Just action -> Action action

evaluate :: Expression -> String
evaluate exp@(Result val@(Concrete _)) = show val
evaluate exp = error "This expression cannot yet be evaluated"

data InterpreterInput = Action InterpreterAction | Evaluate Expression

type InterpreterAction = IO ()
type Value = String

data Expression = ApplyFunction Function Expression | Result Concrete

newtype Concrete = Concrete Value

instance Show Concrete where
  show (Concrete value) = "Concrete: " ++ value

instance Show Expression where
  show (ApplyFunction f parameters) = (show f ++ "applied with: " ++ show parameters)
  show (Result v) = "Result " ++ (show v)

newtype Function = MakeFunction String deriving ( Show )

interpreterActions :: [(String, InterpreterAction)]
interpreterActions = [ ("quit", exitWith ExitSuccess)
                     , ("help", putStrLn "Printing help values")
                     ]