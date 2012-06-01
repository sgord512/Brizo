module Core.Base where

import Control.Monad.Error
import Control.Monad.State
import Data.List ( intersperse )
import Data.Map ( Map )

type Frame = Map String Value
type Scope = [Frame]
type EvaluationContext = State Scope
type EvaluationErrorMonad = ErrorT EvaluationError
type Evaluation = EvaluationErrorMonad EvaluationContext

data EvaluationError = EmptyInputError
                     | UndefinedIdentifierError String
                     | SyntaxError String
                     | ArgumentTypeError String Value
                     | ArgumentArityError Int Int
                     | OtherEvaluationError String
                     | FatalError
                       
instance Error EvaluationError where 
  noMsg = FatalError
  strMsg = OtherEvaluationError
                       
instance Show EvaluationError where
  show EmptyInputError = "No input was provided."
  show (UndefinedIdentifierError s) = "Undefined identifier: " ++ (show s)
  show (SyntaxError s) = "Malformed syntax: " ++ s
  show (ArgumentTypeError e v) = "Incorrect type. Expected: " ++ (show e) ++ " but got: " ++ (show v)
  show (ArgumentArityError e a) = "Expected " ++ (show e) ++ " argument" ++ (if e > 1 then "s" else "") ++ ", but found " ++ (show a)
  show (OtherEvaluationError s) = "Evaluation failed: " ++ s
  show (FatalError) = "Program exited unexpectedly!! No explanation provided."

data Value = SBoolean Bool 
           | SNumber Number 
           | SChar Char 
           | SString String 
           | SSymbol Symbol 
           | SPair Value Value 
           | SProcedure Procedure
           | SIdentifier String
           | SEmptyList

data Procedure = Procedure FormalParameters (Evaluation Value) Bool

type Operation = Expression
type Arguments = [Expression]

type FormalParameters = [String]

data Expression = Expression Value Arguments | Primitive Value

instance Show Expression where
  show (Expression f a) = "( " ++ (show f) ++ " " ++ (concat $ intersperse " " (map show a)) ++ ")"
  show (Primitive v) = show v

instance Show Value where
  show (SBoolean b) = if b then "#t" else "#f"
  show (SNumber n) = show n
  show (SChar c) = "#\\" ++ (c:[]) ++ ""
  show (SString s) = show s
  show (SSymbol s) = '\'':(show s)
  show (SPair car cdr) = "(cons " ++ show car ++ " " ++ show cdr ++ ")"
  show (SProcedure l) = "#<procedure>"
  show (SIdentifier i) = "#<identifier:" ++ i ++ ">"
  show SEmptyList = "()"
  
identifier :: Value -> Bool
identifier (SIdentifier _) = True
identifier _ = False

procedure :: Value -> Bool
procedure (SProcedure _) = True
procedure _ = False

numeric :: Value -> Bool 
numeric (SNumber _) = True
numeric _ = False

newtype Symbol = MakeSymbol String                         
type Number = Integer
                 
instance Show Symbol where 
  show (MakeSymbol s) = s
