module Brizo.Language where

import Data.List
import Data.Map ( Map )
import qualified Data.Map as Map

type InterpreterAction = IO ()
  
newtype Symbol = MakeSymbol String                         
                         

                 
-- Types I need to implement to conform with the R5RS Scheme report                 
-- boolean
-- symbol
-- char
-- vector
-- procedure
-- pair (cons)
-- number
-- string
-- port
-- the empty list is a separate thing, which is not one of any of the above types

data Value = SBoolean Bool 
           | SInteger Integer 
           | SChar Char 
           | SString String 
           | SSymbol String 
           | SPair Value Value 
           | SProcedure Lambda
           | SIdentifier String
           | SEmptyList

data Lambda = Lambda ArgumentList Value

type Operation = Expression

type Parameter = Expression

type ArgumentList = [Expression]

data Expression = Expression Operation ArgumentList | Primitive Value

instance Show Expression where
  show (Expression o a) = "( " ++ (show o) ++ " " ++ (concat $ intersperse " " (map show a)) ++ ")"
  show (Primitive v) = show v

instance Show Value where
  show (SBoolean b) = if b then "#t" else "#f"
  show (SInteger i) = show i
  show (SChar c) = show c
  show (SString s) = show s
  show (SSymbol s) = '\'':(show s)
  show (SPair car cdr) = "(cons " ++ show car ++ " " ++ show cdr ++ ")"
  show (SProcedure l) = "Showing procedure"
  show (SIdentifier i) = "Showing identifier: " ++ i
  show SEmptyList = "()"