module Core.Language where

import Core.Base ( Evaluation )
import Data.List
import Data.Map ( Map )
import qualified Data.Map as Map
import System.Exit

type InterpreterAction = IO ()
                 
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

exit = do putStrLn "Bye. See you soon..."
          exitWith ExitSuccess
