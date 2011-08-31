module Brizo.Parser where

import Brizo.Language
import Control.Monad
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Prim 
import Text.Parsec.String

primitiveSymbols :: [Primitive]
primitiveSymbols = [ Symbol "cons"
                   , Symbol "car"
                   , Symbol "cdr"
                   , Nil
                   ]

expression :: Parser Expression
expression = try (do{ char '(' <?> "open paren"
                    ; c <- cons
                    ; char ')' <?> "close paren"
                    ; return (Exp $ Left c)
                    })
             <|>
             (liftM (\p -> Exp $ Right p)) primitive
             <?> "an expression"

cons :: Parser Cons
cons = do{ string "cons" <?> "cons"
         ; skipMany1 space
         ; p <- primitive <?> "a primitive"
         ; skipMany1 space
         ; e <- expression <?> "an expression"
         ; optional $ skipMany1 space
         ; return $ Cons p e
         }
       <?> "a cons of a primitive and an expression"
       

primitive :: Parser Primitive
primitive = try (string "()" >> return Nil)                   
            <|>
            do{ identifier <- many1 alphaNum
              ; return $ Symbol identifier
              }
            <?> "symbol or empty list \"()\" "

listToExpression :: [Expression] -> Expression
listToExpression [] = Exp $ Right Nil
listToExpression ((Exp e):es) = case e of   
  Right prim -> Exp $ Left $ (Cons prim (listToExpression es))
  Left _ -> error "somehow this list was created incorrectly"