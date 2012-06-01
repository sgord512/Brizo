module Core.Builtin where

import Core.Base
import Core.Evaluator
import Core.Language
import Control.Monad
import Data.Map ( Map )
import qualified Data.Map as Map

formalParameterList = map (\c -> c:[]) ['a'..'z']

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

isPair :: Value -> Value
isPair (SPair v v') = t
isPair _ = f

isNumber :: Value -> Value
isNumber (SNumber i) = t
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

type Predicate = Value -> Value

plus :: Value
plus = let args = take 2 formalParameterList 
       in SProcedure $ Procedure args (do left <- (extractNumber <=< getBinding) (head args)
                                          right <- (extractNumber <=< getBinding) (last args)
                                          return $ SNumber (left + right)) False
          
define :: Value 
define = let args = ["var", "val"]
         in SProcedure $ Procedure args (do var <- (extractIdentifier <=< getBinding) "var"
                                            val <- getBinding "val"
                                            addBinding var val
                                            return $ SIdentifier var) True

predicateToProcedure :: Predicate -> Procedure
predicateToProcedure function = let arg = head formalParameterList 
                                in Procedure [arg] (do val <- getBinding arg
                                                       return $ function val) False
                                   
allBuiltins = Map.fromList (builtins ++ predicates)

builtins = [("+", plus)
           ,("define", define)
           ]
  
predicates = map (\(str, func) -> (str ++ "?", SProcedure $ predicateToProcedure func)) [("boolean", isBoolean)
                                                                                        ,("symbol", isSymbol)
                                                                                        ,("char", isChar)
                                                                                        ,("pair", isPair)
                                                                                        ,("number", isNumber)
                                                                                        ,("procedure", isProcedure)
                                                                                        ]
                                                         