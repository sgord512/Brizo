module Brizo.Base where

import Brizo.Language

t = SBoolean True
f = SBoolean False

-- Functions for classifying the base types

isBoolean :: Value -> Value
isBoolean (SBoolean b) = t
isBoolean _ = f

isSymbol :: Value -> Value
isSymbol (SSymbol s) = t
isSymbol _ = f

isChar :: Value -> Value
isChar (SChar c) = t
isChar _ = f

isBoolean :: Value -> Value
isBoolean (SBoolean b) = t
isBoolean _ = f

isSymbol :: Value -> Value
isSymbol (SSymbol s) = t
isSymbol _ = f

isChar :: Value -> Value
isChar (SChar c) = t
isChar _ = f

isPair :: Value -> Value
isPair (SPair v v') = t
isPair _ = f

isNumber :: Value -> Value
isNumber (SInteger i) = t
isNumber _ = f

isString :: Value -> Value
isString (SString s) = t
isString _ = f

isProcedure :: Value -> Value
isProcedure (SProcedure p) = t
isProcedure _ = f

isNull :: Value -> Value
isNull SEmptyList = t
isNull _ = f

defineSyntax :: [Expression] -> 