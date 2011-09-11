module Brizo.Evaluator where

import Brizo.Language

evaluate :: Expression -> Expression
evaluate e@(Expression o args) = e
evaluate e@(Primitive v) = e

type Frame = Map String Binding


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
