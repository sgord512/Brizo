module Brizo.Parser where

import Brizo.Language
import Control.Monad
import Text.Parsec
import Text.Parsec.Combinator
import Text.Parsec.Prim 
import Text.Parsec.String

-- Whitespace parser
ws = skipMany1 space <?> "space"

backslash = '\\'
hash = '#'
doublequote = '"'

-- Identifier

validChars = ['!','$','%','&','*','+','-','.','/',':','<','=','>','?','@','^','_','~']

identifierStart = (oneOf validChars) <|> letter <?> "beginning of identifier" 

identifierRest = do option "" (many1 ((oneOf validChars) <|> alphaNum))


identifier :: Parser Expression
identifier = do { firstLetter <- identifierStart
                ; rest <- identifierRest
                ; return $ Primitive $ SIdentifier (firstLetter:rest)
                }
             <?> "identifier"

singleExpression :: Parser Expression
singleExpression = stringLiteral
                   <|>
                   numberLiteral
                   <|>
                   try characterLiteral
                   <|> 
                   try booleanLiteral
                   <|>
                   identifier
                   <|> 
                   compoundExpression
                   <?> "literal or expression or identifier"

stringLiteral :: Parser Expression
stringLiteral = do { char doublequote
                   ; contents <- manyTill anyChar (lookAhead $ char doublequote)
                   ; char doublequote
                   ; return $ Primitive $ SString contents
                   }
                <?> "string literal"
                
numberLiteral :: Parser Expression
numberLiteral = do { numString <- many1 digit
                   ; return $ Primitive $ SInteger $ read numString 
                   }
                <?> "number literal"

characterLiteral :: Parser Expression
characterLiteral = do { char hash
                      ; char backslash
                      ; l <- letter
                      ; return $ Primitive $ SChar l
                      }
                   <?> "char literal"

booleanLiteral :: Parser Expression
booleanLiteral = do { char hash
                    ; v <- choice [char 't', char 'f']
                    ; return $ Primitive $ SBoolean $ case v of
                      't' -> True
                      'f' -> False
                    }   
                 <?> "boolean literal"

-- The only way to combine multiple expressions is through 'consing' them

compoundExpression :: Parser Expression
compoundExpression = do{ char '(' <?> "open paren"
                       ; optional ws
                       ; partsList <- singleExpression `sepBy` spaces
                       ; optional ws
                       ; char ')' <?> "close paren"
                       ; return $ case partsList of 
                         [] -> Primitive SEmptyList
                         x:xs -> Expression x xs
                       }
                     <?> "expression"
                     
interpreterExpression :: Parser Expression
interpreterExpression = do { exp <- compoundExpression
                                    <|>
                                    singleExpression
                                    <?> "scheme expression"
                           ; optional ws
                           ; eof
                           ; return exp
                           }