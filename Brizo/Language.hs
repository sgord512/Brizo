module Brizo.Language where


type InterpreterAction = IO ()

data Primitive = Symbol String | Nil
  
newtype Expression = Exp (Either Cons Primitive)

data Cons = Cons Primitive Expression

-- Synonym in case I want to use

type SExp = Expression

-- I'm going to use apostrophe to indicate that something is a symbol
  
instance Show Primitive where
  show (Symbol s) =  "'" ++ show s
  show Nil = "()"

instance Show Expression where
  show (Exp (Left (Cons prim exp))) = "(cons " ++ (show prim) ++ " " ++ (show exp) ++ ")"
  show (Exp (Right p)) = show p

newtype Function = MakeFunction Expression deriving ( Show )

