module Core.Evaluator where

import Core.Base
import Core.Language
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Error 
import Data.List ( find )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromJust, isNothing )

lazyEvaluate :: Expression -> Evaluation Value
lazyEvaluate (Primitive v) = return v
lazyEvaluate e = evaluate e

evaluate :: Expression -> Evaluation Value
evaluate e@(Primitive v) | identifier v = do let (SIdentifier s) = v
                                             value <- getBinding s
                                             return value
                         | otherwise = return v
evaluate e@(Expression f args) | procedure f = do let (SProcedure p) = f
                                                  value <- apply p args
                                                  return value
                               | identifier f = do let (SIdentifier s) = f 
                                                   value <- getBinding s
                                                   evaluatedExpr <- evaluate (Expression value args)
                                                   return evaluatedExpr
                               | otherwise = throwError $ SyntaxError "Expected procedure or identifier"

resolveIdentifier :: String -> Scope -> Maybe Value
resolveIdentifier identifier scope = case (find (Map.member identifier) scope) of
  Just frame -> Just (frame Map.! identifier)
  Nothing -> Nothing
  
getBinding :: String -> Evaluation Value
getBinding identifier = do 
  scope <- get
  case resolveIdentifier identifier scope of
    Just v -> return v
    Nothing -> throwError (UndefinedIdentifierError identifier)
    
addBinding :: String -> Value -> Evaluation ()
addBinding str v = modify (\scope -> (Map.insert str v (head scope)):(tail scope))

pushFrame :: Frame -> Evaluation ()
pushFrame frame = modify (\scope -> frame:scope)

popFrame :: Evaluation Frame
popFrame = do scope <- get
              case scope of 
                (f:fs) -> do put fs
                             return f
                [] -> do throwError $ OtherEvaluationError "Trying to go outside outermost scope"

apply :: Procedure -> [Expression] -> Evaluation Value
apply f arguments = do let (Procedure paramList function lazy) = f
                           (expected, actual) = (length paramList, length arguments)
                       args <- mapM (if lazy then lazyEvaluate else evaluate) arguments
                       unless (expected == actual) (void $ throwError $ ArgumentArityError expected actual)
                       -- unless (all numeric args) (void $ argTypeErr "Number" (fromJust $ find (not . numeric) args))
                       pushFrame $ Map.fromList (zip paramList args)                       
                       result <- function
                       return result
                       
argTypeErr str v = throwError $ ArgumentTypeError str v

extractBool :: Value -> Evaluation Bool
extractBool (SBoolean b) = return b
extractBool _ = return True

extractNumber :: Value -> Evaluation Number
extractNumber (SNumber n) = return n
extractNumber v = argTypeErr "Number" v

extractChar :: Value -> Evaluation Char
extractChar (SChar c) = return c
extractChar v = argTypeErr "Char" v

extractString :: Value -> Evaluation String
extractString (SString s) = return s
extractString v = argTypeErr "String" v

extractIdentifier :: Value -> Evaluation String
extractIdentifier (SIdentifier s) = return s
extractIdentifier v = argTypeErr "Identifier" v

extractSymbol :: Value -> Evaluation Symbol
extractSymbol (SSymbol s) = return s
extractSymbol v = argTypeErr "Symbol" v

extractPair :: Value -> Evaluation (Value, Value)
extractPair (SPair fst snd) = return (fst, snd)
extractPair v = argTypeErr "Pair" v

extractProcedure :: Value -> Evaluation Procedure
extractProcedure (SProcedure p) = return p
extractProcedure v = argTypeErr "Procedure" v

