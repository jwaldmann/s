module Tab.Bin where

import Prelude hiding ( exp )       
import Tab.Exp
import Text.Parsec 
import Control.Applicative ((<*>),(<$>))
       
-- | first parameter: the size n of the table
-- (all numbers are modulo n)   
from :: Int -> [ Bool ] -> Exp
from n s = case parse (exp n) "bin" s of
    Right e -> e

exp n = do
    b <- next
    if not b
    then Const <$> number n
    else IfEq <$> ( toEnum . fromEnum <$> next )
              <*> number n
              <*> exp n
              <*> exp n
    
number 1 = return 0
number n | n > 1 = do
    b <- next
    x <- number $ div n 2
    return $ mod ( 2*x + fromEnum b ) n

next = tokenPrim
     ( \ x -> show x )
     ( \ pos x xs -> pos )
     ( \ x -> Just x )
   <|> return False
    

